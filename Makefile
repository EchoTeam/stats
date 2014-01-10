REBAR= $(shell which ./rebar || which rebar)

.PHONY: all get-deps compile clean test-unit test-ct check

all: get-deps compile

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean
	rm -rf .eunit

test-unit: all
	$(REBAR) eunit skip_deps=true

test-ct: all 
	$(REBAR) ct skip_deps=true

check: test-unit test-ct 

