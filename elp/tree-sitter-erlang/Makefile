# TREE_SITTER=tree-sitter
TREE_SITTER=npm run

all: gen test

.PHONY: test
test: gen
	$(TREE_SITTER) test

.PHONY: gen
gen: deps
	$(TREE_SITTER) generate
	# $(TREE_SITTER) generate  --report-states-for-rule _quoted_atom
	# $(TREE_SITTER) generate  --report-states-for-rule _escape

.PHONY: deps
deps:
	npm ci

.PHONY: t
t: gen
	# $(TREE_SITTER) test -f 'Var'
	# $(TREE_SITTER) test -f 'Unbalanced Quoted Atom'
	$(TREE_SITTER) parse -- foo.elp -d
