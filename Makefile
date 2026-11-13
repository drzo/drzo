GUILE ?= guile
TESTS = series a000081 tree matula butcher psystem legacy

.PHONY: check demo clean

check:
	@status=0; \
	for t in $(TESTS); do \
	  GUILE=$(GUILE) $(GUILE) -L . tests/$$t.scm || status=1; \
	done; \
	exit $$status

demo:
	$(GUILE) -e main -s bin/ghost demo

clean:
	rm -f *.log
