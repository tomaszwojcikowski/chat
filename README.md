[![Build Status](https://travis-ci.org/tomaszwojcikowski/chat.svg?branch=master)](https://travis-ci.org/tomaszwojcikowski/chat)

chat
=====

An OTP application

Build
-----

    $ rebar3 compile
    
Run
----

    chat_server:start_link().
    {ok, Client} = chat_client:start_link("Your Name").
    chat_client:connect(Client, chat_server).
    chat_client:send(Client, Username, Message).

