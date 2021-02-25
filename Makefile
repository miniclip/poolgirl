SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

version: upgrade clean compile check test edoc
.PHONY: version

upgrade: upgrade-rebar3_lint
	@rebar3 do unlock,upgrade
.PHONY: upgrade

upgrade-rebar3_lint:
	@rebar3 plugins upgrade rebar3_lint
.PHONY: upgrade-rebar3_lint

clean:
	@rebar3 clean -a
.PHONY: clean

compile:
	@rebar3 compile
.PHONY: compile

check: xref dialyzer elvis-rock
.PHONY: check

xref:
	@rebar3 as test xref
.PHONY: xref

dialyzer:
	@rebar3 as test dialyzer
.PHONY: dialyzer

elvis-rock:
	@rebar3 lint
.PHONY: elvis-rock

test: eunit cover
.NOTPARALLEL: test
.PHONY: test

eunit:
	@rebar3 eunit
.PHONY: eunit

cover:
	@rebar3 cover
.PHONY: cover

edoc:
	@rebar3 edoc
.PHONY: edoc

bench/basho_bench:
	@git clone git://github.com/basho/basho_bench.git bench/basho_bench || true
	# link the basho_bench driver
	@rm -f bench/basho_bench/src/basho_bench_driver_poolgirl.erl
	@ln -s ../../basho_bench_driver_poolgirl.erl \
		  bench/basho_bench/src/basho_bench_driver_poolgirl.erl
	@cd bench/basho_bench; rebar3 escriptize; cd ..

bench: bench/basho_bench compile
	@cd bench; \
		rm -rf basho_bench/tests; \
		basho_bench/basho_bench \
			--results-dir basho_bench/tests \
			basho_bench_driver_poolgirl.config; \
		cd basho_bench; make results; \
		cp tests/current/summary.png poolgirl.summary.`date +%d%b%Y-%H%M%S`.png; \
		rm ../../src/poolgirl_test_worker.erl
