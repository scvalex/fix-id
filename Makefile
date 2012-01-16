all: build

.PHONY: all build clean

build: deps
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean
	rm -rf deps
