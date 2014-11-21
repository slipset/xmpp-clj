# xmpp-clj

xmpp-clj allows you to write simple jabber bots in idiomatic clojure by providing a lightweight wrapper around the [smack](http://www.igniterealtime.org/projects/smack/) library.

This version is compatible with hipchat from atlassian.

## Lein

[![Clojars Project](http://clojars.org/net.assum/xmpp-clj/latest-version.svg)](http://clojars.org/net.assum/xmpp-clj)

## Usage

Create a temporary jabber account for your bot.  I've used gmail here, but there are a bunch of free providers
<br />

Create a leiningen project and cd into the project directory

    lein new mybot
    cd ./mybot
<br />

Add xmpp-clj to your deps (project.clj):

    (defproject mybot "0.1.0"
      :description "FIXME: write"
      :dependencies [[net.assum/xmpp-clj "0.0.1"]])
<br />

Open up src/mybot/core.clj and require the xmpp lib:

    (ns mybot.core
      (:require [xmpp-clj.bot :as xmpp]))
<br />

Define a handler and start the bot. Handlers accept a single parameter
-- the message map -- and should return a string with a message back
to the sender. Return `nil` to omit a response.  Here's a very simple
example:

    ;; This bot does not respond.

    (def config {:host "localhost"
                 :port 5222
                 :username "sausagebot"
                 :domain "localhost"
                 :password "sausage"
                 :nick "sausagebot"
                 :resource "Mr. Sausage"
                 :room "clojure@conference.clojutre"})

    (def chat (xmpp/start config))

    (def room (xmpp-clj/join chat (:room config) (:nick config)))

    (.sendMessage clojure-room "clojure rocks")

Next, fire up your chat client, add your new bot buddy, and send him /
her a message.  The response should look someting like this:

See the `src/xmpp_clj/examples` folder for additional examples,
including MUC chat.

## Debugging

Use `-Dsmack.debugEnabled=true` to enable xmpp protocol output.

## Problems?

Open up an [issue](http://github.com/slipset/xmpp-clj/issues)

## License

[Eclipse Public License v1.0](http://www.eclipse.org/legal/epl-v10.html)
