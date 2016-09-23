TESTS=$(sort $(patsubst %.expr,%,$(wildcard test???.expr)))

.PHONY: check $(TESTS) list_tests
check: $(TESTS)

list_tests:
	@echo $(TESTS)

$(TESTS): %: %.expr %.input orig/%.log
	../src/rc.byte $@.expr && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm *.log *.s *~
