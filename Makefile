VERSION ?=
CMD ?=

EMACS ?= emacs

# The order is important for compilation.
#
# Get all files for which we might wish to test compilation, then
# remove files that we shouldn't test based on the Emacs version.
for_compile := prescient.el $(wildcard *-prescient.el)
ifneq ($(strip $(VERSION)),)
  ifeq (1, $(strip $(shell expr $(VERSION) \< 26)))
    for_compile := $(filter-out selectrum-prescient.el,$(for_compile))
  endif
  ifeq (1, $(strip $(shell expr $(VERSION) \< 27)))
    for_compile := $(filter-out vertico-prescient.el corfu-prescient.el,\
                                $(for_compile))
  endif
endif
for_checkdoc := prescient.el $(wildcard *-prescient.el)
for_longlines := $(wildcard *.el *.md *.yml) Makefile

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2

.PHONY: lint
lint: compile checkdoc longlines test ## Run all the linters and tests

.PHONY: compile
compile: ## Byte-compile
	@for file in $(for_compile); do \
	    echo "[compile] $$file" ;\
	    $(EMACS) -Q --batch -L . -L stub -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkdoc
checkdoc: ## Check docstring style
	@for file in $(for_checkdoc); do \
	    echo "[checkdoc] $$file" ;\
	    $(EMACS) -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(setq sentence-end-double-space nil)" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: longlines
longlines: ## Check for long lines
	@scripts/check-line-length.bash

.PHONY: test
test:
	$(EMACS) -Q --batch -L . -l ert -l ./test/prescient-test.el \
		 --eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))"

.PHONY: clean
clean: ## Remove build artifacts
	@echo "[clean]" *.elc
	@rm -f *.elc

.PHONY: docker
docker: ## Start a Docker shell; e.g. make docker VERSION=25.3
	@scripts/docker.bash "$(VERSION)" "$(CMD)"
