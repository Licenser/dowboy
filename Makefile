
.PHONY: all console deps

all: deps
	./rebar compile

deps:
	./rebar get-deps

console: all
	 erl -pa deps/*/ebin ebin -s dowboy

rel: all
	./rebar generate
