(ns xmpp-clj.bot-test
    (:require [clojure.test :refer :all]
              [midje.sweet  :refer :all]   
              [xmpp-clj.bot :refer :all])
    (:import [org.jivesoftware.smack
            Chat ChatManager ConnectionConfiguration MessageListener
            SASLAuthentication XMPPConnection XMPPException PacketListener]
           [org.jivesoftware.smack.packet
            Message Presence Presence$Type Message$Type]
           [org.jivesoftware.smack.filter MessageTypeFilter]
           [org.jivesoftware.smack.util StringUtils]
           [org.jivesoftware.smackx.muc
            MultiUserChat DiscussionHistory InvitationListener]))

(facts test-create-message
  (fact "that it works"
    (let [to "myuser"
          type Message$Type/chat
          body "foo"
          message (create-message :from {:from to :type type :response body})]
      (.getBody message) => body 
      (.getType message) => type
      (.getTo message) => to)))
      
(facts test-wrap-responder
       (fact "that sender is called when a message is returned"
             (let [fn (wrap-responder (fn [conn msg] "foo") (fn [conn msg] msg))]
               (function? fn) => true
               (:response (fn nil {})) => "foo"))
  
  (fact "that sender is not called when a message is not returned"
    (let [fn (wrap-responder (fn [conn msg] nil) #(%1 %2))]
      (function? fn) => true
      (fn nil {}) => nil)))

(facts test-connection-type
       (fact "that connection-type returns the correct type"
             (connection-type "s" :a :b :c) => String))


(facts test-cfg->conn
  (fact "that we're getting the correct stuff back"
    (type (cfg->conn {:host "foo" :domain "bar"})) => XMPPConnection))

(facts test-connect
  (fact"that an exception is thrown if required params are not present"
    (connect {}) => (throws Exception)) 

  (fact "that the connect function works as expected"
    (with-redefs-fn {#'do-connect (fn [conn credentials] credentials)}
      #(let [cfg {:username "foo"
                  :password "bar"
                  :host "localhost"
                  :domain "localhost"}]
         (connect cfg) => cfg))))

(facts test-create-sender
  (fact "that it works"
    (with-redefs-fn {#'send-message (fn [conn msg] msg)}
      #(let [fun (create-sender identity)]
         (fun nil 1) => 1))))

(facts test-wrap-remove-connection
  (fact "that it removes the connection in the handler chain"
    (let [fun (wrap-remove-connection identity)]
      (fun nil 1) =>  1)))

(facts test-wrap-remove-message
  (fact "that wrap-remove-message works"
    (let [fun (wrap-remove-message identity (fn [m] (= m 2)))]
      (fun 1) => 1
      (fun 2) => nil)))

(facts test-with-message-map
  (fact "that we get a map from a message"
    (let [msg (create-message :from {:response "foo" :type Message$Type/chat})
          mapfn (with-message-map (fn [conn msg] msg))
          result (mapfn nil msg)]
      (:body result) => "foo"
      (:type result) => Message$Type/chat)))
