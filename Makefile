all: compile

compile:
	./rebar  compile
tests:
	./rebar  qc eunit suite=ecbreak_tests
check:
	./rebar xref dialyze
prepare-check:
	./rebar -v build-plt
doc:
	./rebar  doc
clean:
	./rebar clean
etags:
	find src include test -name "*.[he]rl" -print | etags -

.PHONY: doc
