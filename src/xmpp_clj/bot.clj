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
  (first (.split from "/")))

(defn create-message [from-field msg]
  (doto (Message. (from-field msg) Message$Type/chat)
    (.setBody (str (:response msg)))
    (.setType (:type msg))))
  
(defn wrap-responder [handler sender]
  (fn [conn message]
    (some->> (handler conn message)
    (assoc message :response)
    (sender conn ))))

(defn with-message-map [handler]
  (fn [conn packet]
    (->> packet
     (message->map)
     (handler conn))))

(defn wrap-remove-connection [handler]
  (fn [conn message]
    (handler message)))

(defn wrap-tee [handler f]
  (fn [message]
    (f message)
    (handler message)))

(defn wrap-remove-message [handler p]
  (fn [message]
    (when-not (p message)
      (handler message))))

(defn wrap-errors [out]
  (fn [handler]
    (fn [conn packet]
      (try 
        (handler conn packet)
        (catch Exception e
          (.println out "Got an error")
          (.printStackTrace e out))))))

(defn- noop [handler]
  (fn [conn packet]
    (handler conn packet)))

(defn dev-null []
  (fn [conn msg]))

(defn default-processor [in out & [error-handler]]
  (let [wrap-errors (or error-handler noop)]
    (-> in
        (wrap-remove-connection)
        (wrap-responder out)
        (with-message-map)
        (wrap-errors))))

(defn connection-type [conn & rest]
  (type conn))

(defmulti send-message connection-type)

(defmethod send-message XMPPConnection [conn resp]
  (.sendPacket conn resp))

(defmethod send-message MultiUserChat [conn resp]
  (.sendMessage conn resp))

(defn create-sender [message-creator]
  (fn [conn message]
    (->> message
         (message-creator)
         (send-message conn))))

(defmulti add-listener connection-type)

(defmethod add-listener XMPPConnection [conn processor type-filter]
  (.addPacketListener
    conn
    (packet-listener conn processor)
    type-filter))

(defmethod add-listener MultiUserChat [conn processor]
  (.addMessageListener
    conn
    (packet-listener conn processor)))

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


(defn cfg->conn [{:keys [host port domain] :or {port 5222}}]
  (-> (ConnectionConfiguration. host port domain)
      (XMPPConnection.)))

(defn format-error [username password]
  (str "Couldn't log in with user's credentials: "
       username
       " / "
       (apply str (take (count password) (repeat "*")))))

(defn do-connect [conn {:keys [username password]}]
    (.connect conn)
    (try
      (.login conn username password)
      (.sendPacket conn available-presence)
      conn
      (catch XMPPException e
        (throw (Exception. (format-error username password))))))
  
(defn connect [{:keys [username password host domain port] :as config}]
  (if-not (and username password host domain)
    (throw (Exception. "Required connection params not provided (:username :password :host :domain)")))
  (-> (cfg->conn config)
      (do-connect config)))

(defn join [conn room nick]
  (let [muc (MultiUserChat. conn room)]
    (.join muc nick nil no-history muc-join-timeout-ms)
    muc))

(defn stop [#^XMPPConnection conn]
  (when conn
    (.disconnect conn)))
