(ns xmpp-clj.bot-test
    (:require [clojure.test :refer :all]
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

(deftest test-create-message
  (testing "that it works"
    (let [to "myuser"
          type Message$Type/chat
          body "foo"
          message (create-message to type body)]
      (is (= body (.getBody message)))
      (is (= type (.getType message)))
      (is (= to (.getTo message))))))
      
(deftest test-wrap-responder
  (testing "that sender is called when a message is returned"
    (let [fn (wrap-responder #(% "foo") (fn [conn msg] msg))]
      (is (function? fn))
      (is (= (:response (fn nil {}) "foo")))))
  
  (testing "that sender is not called when a message is not returned"
    (let [fn (wrap-responder #(% nil) #(%1 %2))]
      (is (function? fn))
      (is (not (fn nil {}))))))

(deftest test-connection-type
  (testing "that connection-type returns the correct type"
    (is (= String (connection-type "s" :a :b :c)))))
             
      
    
  
          

    
