REBAR=./rebar

all: deps compile edoc

run:
	erl -pa deps/mochiweb/ebin -pa deps/mochiweb_xpath/ebin -pa ebin -sname master -s webscraping_app

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

edoc:
	@$(REBAR) doc skip_deps=true

clean:
	@$(REBAR) clean

.PHONY: deps
