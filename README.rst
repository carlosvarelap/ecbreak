======================
Erlang Circuit Breaker
======================

This library implements the Circuit Breaker pattern as described in *Michael T. Nygard, "Release It! Design and Deploy Production-Ready Software"*

Setting up the enviroment
=========================
The only thing you have to do to setup your environment is to call the configure script (configure.sh) specifying your Erlang OTP base path. for example::

  ./configure.sh /home/cvarela/work/Erlang_R14A-x86

If you don't have a full QuickCheck version,you can download and install one by executing::

  ./utils/get_eqcmini.sh

Building the library
====================

The library is built using rebar. If you are not familiar with rebar, you have a good starting point in https://bitbucket.org/basho/rebar/wiki. Anyway, to compile you should use::

  ./rebar compile

And to run the tests::

  ./rebar qc eunit



