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
          message (create-message :from {:from to :type type :response body})]
      (is (= body (.getBody message)))
      (is (= type (.getType message)))
      (is (= to (.getTo message))))))
      
(deftest test-wrap-responder
  (testing "that sender is called when a message is returned"
    (let [fn (wrap-responder (fn [conn msg] "foo") (fn [conn msg] msg))]
      (is (function? fn))
      (is (= (:response (fn nil {}) "foo")))))
  
  (testing "that sender is not called when a message is not returned"
    (let [fn (wrap-responder (fn [conn msg] nil) #(%1 %2))]
      (is (function? fn))
      (is (not (fn nil {}))))))

(deftest test-connection-type
  (testing "that connection-type returns the correct type"
    (is (= String (connection-type "s" :a :b :c)))))

(deftest test-cfg->conn
  (testing "that we're getting the correct stuff back"
    (is (= (type (cfg->conn {:host "foo" :domain "bar"})) XMPPConnection))))

(deftest test-connect
  (testing "that an exception is thrown if required params are not present"
    (is (thrown? Exception (connect {}))))

  (testing "that the connect function works as expected"
    (with-redefs-fn {#'do-connect (fn [conn credentials] credentials)}
      #(let [cfg {:username "foo"
                  :password "bar"
                  :host "localhost"
                  :domain "localhost"}]
         (is (= (connect cfg) cfg))))))

(deftest test-create-sender
  (testing "that it works"
    (with-redefs-fn {#'send-message (fn [conn msg] msg)}
      #(let [fun (create-sender identity)]
         (is (= (fun nil 1) 1))))))

(deftest test-wrap-remove-connection
  (testing "that it removes the connection in the handler chain"
    (let [fun (wrap-remove-connection identity)]
      (is (= (fun nil 1) 1)))))

(deftest test-wrap-remove-message
  (testing "that wrap-remove-message works"
    (let [fun (wrap-remove-message identity (fn [m] (= m 2)))]
      (is (= (fun 1) 1))
      (is (= (fun 2) nil)))))

(deftest test-with-message-map
  (testing "that we get a map from a message"
    (let [msg (create-message :from {:response "foo" :type Message$Type/chat})
          mapfn (with-message-map (fn [conn msg] msg))
          result (mapfn nil msg)]
      (is (= (:body result) "foo"))
      (is (= (:type result) Message$Type/chat)))))
