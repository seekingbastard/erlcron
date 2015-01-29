#!/bin/sh
exec erl -args_file env/erlcron.vmargs \
	-pa ebin/ deps/*/ebin \
	-config env/erlcron \
    -boot start_sasl \
    -s reloader \
    -s erlcron 
