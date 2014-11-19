(ns xmpp-clj.bot
  (:import [org.jivesoftware.smack
            Chat ChatManager ConnectionConfiguration MessageListener
            SASLAuthentication XMPPConnection XMPPException PacketListener]
           [org.jivesoftware.smack.packet
            Message Presence Presence$Type Message$Type]
           [org.jivesoftware.smack.filter MessageTypeFilter]
           [org.jivesoftware.smack.util StringUtils]
           [org.jivesoftware.smackx.muc
            MultiUserChat DiscussionHistory InvitationListener]))

(def available-presence (Presence. Presence$Type/available))

(def chat-message-type-filter (MessageTypeFilter. Message$Type/chat))
(def groupchat-message-type-filter (MessageTypeFilter. Message$Type/groupchat))

(def no-history
  (doto (DiscussionHistory.) (.setMaxChars 0)))

(def muc-join-timeout-ms (long 10000))


(defn connection-type [conn & rest] (type conn))
  
(defn packet-listener [conn processor]
  (proxy
    [PacketListener]
    []
    (processPacket [packet] (processor conn packet))))

(defn error->map [e]
  (if (nil? e)
    nil
    {:code (.getCode e) :message (.getMessage e)}))

(defn extract-delay [#^Message m]
  (some-> (.getExtension m "urn:xmpp:delay") (.getStamp)))

(defn message->map [#^Message m]
  {:body (.getBody m)
   :subject (.getSubject m)
   :thread (.getThread m)
   :from (.getFrom m)
   :from-name (StringUtils/parseBareAddress (.getFrom m))
   :from-nick (StringUtils/parseResource (.getFrom m))
   :to (.getTo m)
   :packet-id (.getPacketID m)
   :error (error->map (.getError m))
   :type (.getType m)
   :delay (extract-delay m)})

(defn parse-address [from]
  (first (.split from "/"))

(defn create-message [to type to-message-body]
  (doto (Message. to Message$Type/chat)
    (.setBody (str to-message-body))
    (.setType type)))
  
(defn wrap-responder [handler sender]
  (fn [conn message]
    (if-let [resp (handler message)]
      (sender conn (assoc message :response resp)))))

(defn wrap-debug [handler out]
  (fn [m]
    (.println out m)
    (handler m)))

(defmulti send-message connection-type)

(defmethod send-message XMPPConnection [conn resp]
  (.sendPacket conn resp))

(defmethod send-message MultiUserChat [conn resp]
  (.sendMessage conn resp))

(defn with-message-map [handler]
  (fn [conn packet]
    (let [message (message->map #^Message packet)]
        (handler conn message))))

(defn create-packet-listener [conn error-handler message-handler sender ]
  (packet-listener conn (error-handler 
                         (with-message-map
                           (wrap-responder message-handler sender)))))

(defmulti default-sender connection-type)

(defmethod default-sender XMPPConnection [conn address-field]
  (fn [conn message]
    (send-message conn (create-message (address-field message) (:type message) (:response message)))))

(defmethod default-sender MultiUserChat [conn ]
  (fn [conn message]
    (send-message conn (:response message))))

(defn default-error-handler [handler]
  (let [out *out*]
    (fn [conn packet]
      (try 
        (handler conn packet)
        (catch Exception e
          (.println out "Got an error")
          (.printStatckTrace e out))))))

(defmulti add-listener connection-type)

(defmethod add-listener XMPPConnection [conn message-handler type-filter address-field & [message-sender error]]
  (let [sender (or message-sender (default-sender conn address-field))
        error-handler (or error default-error-handler)]
    (.addPacketListener
      conn
      (create-packet-listener conn error-handler message-handler sender )
      type-filter)))

(defmethod add-listener MultiUserChat [conn message-handler & [message-sender error]]
  (let [sender (or message-sender (default-sender conn))
        error-handler (or error default-error-handler)]
    (.addMessageListener conn (create-packet-listener conn error-handler message-handler sender))))

(defn- create-invitation-message [room inviter reason password message]
  {
   :room room
   :inviter inviter
   :reason reason
   :password password
   :message message
   })

(defn add-invitation-listener [conn handler]
  (let [listener (reify InvitationListener
                   (invitationReceived [this conn room inviter reason password message]
                     (handler (create-invitation-message room inviter reason password message))))]
    (MultiUserChat/addInvitationListener conn listener)))

(defn decline-invitation [conn invitation reason]
  (MultiUserChat/decline conn
                         (:room invitation)
                         (:inviter invitation)
                         reason))

(defn listen [connection packet-processor]
  (add-listener connection packet-processor chat-message-type-filter :from)
  connection)

(defn connect
  [connect-info]
  (let [un (:username connect-info)
        pw (:password connect-info)
        host (:host connect-info)
        domain (:domain connect-info)
        port (get connect-info :port 5222)
        connect-config (ConnectionConfiguration. host port domain)
        conn (XMPPConnection. connect-config)]
    (if-not (and un pw host domain)
      (throw (Exception. "Required connection params not provided (:username :password :host :domain)")))
    (.connect conn)
    (try
      (.login conn un pw)
      (catch XMPPException e
        (throw (Exception. (str "Couldn't log in with user's credentials: "
                                un
                                " / "
                                (apply str (take (count pw) (repeat "*"))))))))
    (.sendPacket conn available-presence)
    conn))

(defn start
  "Defines and starts an instant messaging bot that will respond to incoming
   messages. `start` takes 2 parameters, the first is a map representing
   the data needed to make a connection to the jabber server:

   connnect-info example:
   {:host \"talk.google.com\"
  :domain \"gmail.com\"
  :username \"testclojurebot@gmail.com\"
  :password \"clojurebot12345\"}

   The second parameter expects a single-arg function, which is passed
   a map representing a message on receive. Return a string from this
   function to pass a message back to the sender, or nil for no
   response

   received message map example (nils are possible where n/a):
   {:body
   :subject
   :thread <Id used to correlate several messages, such as a converation>
   :from <entire from id, ex. zachary.kim@gmail.com/Zachary KiE0124793>
   :from-name <Just the 'name' from the 'from', ex. zachary.kim@gmail.com>
   :to <To whom the message was sent, i.e. this bot>
   :packet-id <donno>
   :error <a map representing the error, if present>
   :type <Type of message: normal, chat, group_chat, headline, error.
   see javadoc for org.jivesoftware.smack.packet.Message>}
   "
  [connect-info & [packet-processor]]
  (let [connection (connect connect-info)]
    (if packet-processor
      (listen connection packet-processor)
      connection)))

(defn join [conn room nick]
  (let [muc (MultiUserChat. conn room)]
    (.join muc nick nil no-history muc-join-timeout-ms)
    muc))
    
(defn start-muc
  [connect-info packet-processor]
  (let [room (:room connect-info)
        nick (:nick connect-info)
        actual-nick (or nick (:username connect-info))]
    (if-not room
      (throw (Exception. "Require a room to join.")))
    (let [conn (connect connect-info)
          muc (join conn room nick)]
      (add-listener conn packet-processor groupchat-message-type-filter :from)
      muc)))

(defn stop [#^XMPPConnection conn]
  (when conn
    (.disconnect conn)))
