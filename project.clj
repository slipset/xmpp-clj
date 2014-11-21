(defproject net.assum/xmpp-clj "0.0.1"
  :description "A lightweight clojure wrapper around the smack jabber/XMPP library. Hipchat compatible."
   :repl-options {
                  :init-ns xmpp-clj.bot
                  }

  :dependencies [[org.clojure/clojure "1.6.0"]
		 [jivesoftware/smack "3.1.0"]
                 [jivesoftware/smackx "3.1.0"]
		 ])
