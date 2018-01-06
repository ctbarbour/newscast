========
Newscast
========

Copyright (c) 2018 Tucker Barbour

**Authors**: Tucker Barbour (ctbarbour [at] gmail (dot) com)

Intro
-----
This is an Erlang/OTP implementation of the Newscast gossip protocol. The Newscast model is a general approach for communication in large agent-based distributed systems. Newscast solves two main problems, information dissemination and membership management, in a highly reliable and fully distributed manner allowing the model to scale to extremely large networks.

Use Cases
---------

What can Newscast be used for?

* Multicast
* Pub/Sub
* Distributed Computing

Newscast can be used as the basis for any application which requires dissemination-oriented communication.

Why?
----

Why Not?
--------

How To
------

.. code-block:: erlang

  application:start(newscast),
  newscast:join({{127,0,0,1}, 6001}).

Build
-----

Newscast requires OTP-17 or greater and rebar3.

.. code-block:: bash

   rebar3 do xref, dialyzer, ct
