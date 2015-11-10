.PHONY: all test test-deps clean

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

clean:
	@./rebar clean

PLT ?= .dialyzer.plt

# Builds dialyzer's Persistent Lookup Table file.
.PHONY: plt
plt:
	dialyzer --check_plt --plt ${PLT}; \
	if [ $$? != 0 ]; then \
	    dialyzer --build_plt --output_plt ${PLT} --apps kernel stdlib sasl erts \
	        ssl tools runtime_tools crypto inets xmerl snmp public_key eunit \
	        common_test test_server syntax_tools compiler ./deps/*/ebin; \
	fi; exit 0

# Dialyzes the project.
dialyzer: plt
	dialyzer ./ebin --plt ${PLT} -Werror_handling -Wrace_conditions --fullpath


test-deps:
	@if [ ! -d "test-deps/cowboy" ]; then \
		git clone -b websocket_payload_nif https://github.com/RoXeon/cowboy.git test-deps/cowboy; \
	fi
	@cd test-deps/cowboy && ../../rebar get-deps && ../../rebar compile

test: compile test-deps
	mkdir -p .ct_results
	ct_run -pa test-deps/cowboy/ebin test-deps/cowboy/deps/ranch/ebin test-deps/cowboy/deps/cowlib/ebin ebin \
	-dir ct \
	-logdir ./.ct_results \
	-cover ct/websocket_client.coverspec
