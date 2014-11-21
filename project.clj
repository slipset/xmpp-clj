(defproject net.assum/xmpp-clj "0.0.2"
  :description "A lightweight clojure wrapper around the smack jabber/XMPP library. Hipchat compatible."
   :repl-options {:init-ns xmpp-clj.bot}

  :dependencies [[org.clojure/clojure "1.6.0"]
		 [org.igniterealtime.smack/smack "3.2.1"]
                 [org.igniterealtime.smack/smackx "3.2.1"]]
   :profiles {:dev {:dependencies [[midje "1.6.3"]]}}
    :scm {:name "git"
          :url "https://github.com/slipset/xmpp-clj.git"})

