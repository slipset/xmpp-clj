(ns xmpp_clj.examples.basic
   (:require [xmpp-clj.bot :as xmpp]))

(def config {:host "localhost"
             :port 5222
             :username "sausagebot"
             :domain "localhost"
             :password "sausage"
             :nick "sausagebot"
             :resource "Mr. Sausage"
             :room "clojure@conference.clojutre"})


(defn from-me? [{:keys [from]}]
  (.contains from (str (:room config) "/" (:nick config))))

(defn handle-chatter [msg]
  (let [body {:body msg}]
    (.contains body "clojure" "Clojure Rocks!")))

(def msgs (atom []))

(defn store-message [message] (swap! msgs conj message))

(def message-listener (-> handle-chatter
                          (xmpp/wrap-tee store-message)
                          (xmpp/wrap-remove-message from-me?)))
(comment 
  (reset! msgs [])
  (.disconnect chat)
  (def out *out*)
  (def chat (xmpp/start config))

  (.sendMessage clojure-room "clojure rocks")

  (xmpp/add-listener chat (xmpp/default-processor
                            #'message-listener
                              (xmpp/create-sender (partial xmpp/create-message :from-name))
                              (xmpp/wrap-errors out)) org.jivesoftware.smack.packet.Message$Type/chat)
)
