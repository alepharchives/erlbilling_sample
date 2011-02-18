#!/bin/sh

exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname billing_dev \
    -s billing \
    -s reloader \
    -mnesia '"db"'
