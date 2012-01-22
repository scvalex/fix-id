DB_DIR := "/tmp/fix_id"

all: build

.PHONY: all run build deps release clean squeaky-clean cleandb

run: build
	erl -pa ebin \
	    -pa deps/*/ebin \
	    -sname fix_id \
	    -config dev.config \
	    -boot start_sasl \
	    -eval 'application:start(lager).' \
	    -eval 'application:start(gen_smtp).' \
	    -eval 'application:start(mnesia).' \
	    -eval 'application:start(fix_id).'

build: deps
	rebar compile

deps:
	rebar get-deps

release: build
	[ ! -e 'rel/fix_id/bin/fix_id' ] && \
		rebar generate || \
		{ echo 'Release already exists; run `make clean` first' ; \
		  false ; }

clean:
	rebar clean
	find . -name erl_crash.dump -exec rm '{}' \;

squeaky-clean: clean
	rm -rf deps

cleandb:
	rm -rf ${DB_DIR}
