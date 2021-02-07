# Copyright 2010-2020 Manolis Papadakis <manopapad@gmail.com>,
#                     Eirini Arvaniti <eirinibob@gmail.com>
#                 and Kostis Sagonas <kostis@cs.ntua.gr>
#
# This file is part of PropEr.
#
# PropEr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# PropEr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

# Author(s):   Manolis Papadakis and Kostis Sagonas
# Description: Instructions for make

.PHONY: default all compile dialyzer check_escripts test doc clean distclean rebuild retest

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

REBAR3_URL := https://s3.amazonaws.com/rebar3/rebar3
REBAR3 ?= $(shell which rebar3 || which .$(SEP)rebar3 || \
            (wget $(REBAR3_URL) && chmod +x rebar3 && echo .$(SEP)rebar3))
COVER ?= false

default: compile

all: compile dialyzer doc test

compile:
	$(REBAR3) compile

dialyzer: .plt
	$(REBAR3) dialyzer

check_escripts:
	./scripts/check_escripts.sh make_doc

test:
ifeq ($(COVER), true)
	$(REBAR3) do eunit -c, cover, covertool generate
else
	$(REBAR3) eunit
endif

test-examples:
	$(REBAR3) eunit --dir=examples_test

doc:
	$(REBAR3) edoc

clean:
	./scripts/clean_temp.sh

distclean: clean
	$(REBAR3) clean
	$(RM) .plt/*_plt
	$(RM) -r _build rebar3 rebar.lock

rebuild: distclean compile

retest:
	$(RM) -r _build/test/lib/proper/test
	$(REBAR3) eunit
