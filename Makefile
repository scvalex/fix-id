all: build

.PHONY: all run build deps generate clean

run: build
	erl -pa ebin -pa deps/*/ebin -boot start_sasl -sasl sasl_error_logger '{file, "/tmp/fix_id.log"}' -eval 'fix_id_app:start(normal, []).'

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
