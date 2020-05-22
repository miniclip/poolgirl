# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

DEPS_PLT=./.deps_plt
DEPS=kernel stdlib

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
REBAR=$(shell which rebar)

.PHONY: all compile clean dialyze rebuild test

all: compile
travis: dialyze test

# =============================================================================
# Rules to build the system
# =============================================================================

compile:
	- $(REBAR) skip_deps=true compile

$(DEPS_PLT):
	@echo Building $(DEPS_PLT)
	- dialyzer --build_plt \
	   --output_plt $(DEPS_PLT) \
	   --apps $(DEPS)

dialyze: $(DEPS_PLT) compile
	- dialyzer --fullpath \
		--src src \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		-Wunderspecs \
		-r ./ebin \
		--plt $(DEPS_PLT)

test: compile
	$(REBAR) eunit

clean:
	- $(REBAR) skip_deps=true clean
	- rm -rf ebin

rebuild: clean compile

src/poolgirl_test_worker.erl:
	cp test/poolgirl_test_worker.erl src/poolgirl_test_worker.erl

bench: bench/basho_bench src/poolgirl_test_worker.erl compile
	cd bench; \
		rm -rf basho_bench/tests; \
		basho_bench/basho_bench \
			--results-dir basho_bench/tests \
			basho_bench_driver_poolgirl.config; \
		cd basho_bench; make results; \
		cp tests/current/summary.png poolgirl.summary.`date +%d%b%Y-%H%M%S`.png; \
		rm ../../src/poolgirl_test_worker.erl

bench/basho_bench:
	git clone git://github.com/basho/basho_bench.git bench/basho_bench || true
	# link the basho_bench driver
	rm -f bench/basho_bench/src/basho_bench_driver_poolgirl.erl
	ln -s ../../basho_bench_driver_poolgirl.erl \
		  bench/basho_bench/src/basho_bench_driver_poolgirl.erl
	cd bench/basho_bench; ./rebar get-deps compile escriptize; cd ..
