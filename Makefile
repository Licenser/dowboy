
.PHONY: all console deps

deps:
	./rebar get-deps

all: deps
	./rebar compile

console: all
	 erl -pa deps/*/ebin ebin -s dowboy
