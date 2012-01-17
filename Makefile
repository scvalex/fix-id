all: build

.PHONY: all run build deps generate clean

run: build
	erl -pa ebin \
	    -pa deps/*/ebin \
	    -sname fix_id \
	    -boot start_sasl \
	    -sasl sasl_error_logger '{file, "/tmp/fix_id.log"}' \
	    -eval 'application:start(gen_smtp).' \
	    -eval 'application:start(fix_id).'

build: deps
	rebar compile

deps:
	rebar get-deps

generate: build
	rebar generate

clean:
	rebar clean
	find . -name erl_crash.dump -exec rm '{}' \;

squeaky-clean: clean
	rm -rf deps
