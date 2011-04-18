#!/bin/sh
mkdir -p ext_libs
cd ext_libs
git clone git://github.com/basho/rebar.git
cd rebar
git checkout ta-qc
PATH=$OTP_DIR/bin:$PATH ./bootstrap
cp -i rebar $ECBREAK_DIR
