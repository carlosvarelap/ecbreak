all: compile

compile:
	./rebar  compile
tests:
	./rebar  qc eunit suite=ecbreak_tests
doc:
	./rebar  doc
clean:
	./rebar clean

.PHONY: doc
