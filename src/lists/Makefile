BUILD_DIR = build
LISTS_SOURCES = lists.mli lists.ml
LISTS_EXEC = lists
TESTS_SOURCES = tests.ml
TESTS_EXEC = tests
TESTS_DIR = tests

all: post-build
debug: debug-post-build

post-build: main-build
debug-post-build: debug-main-build

main-build: pre-build
	@$(MAKE) --no-print-directory lists

debug-main-build:
	@$(MAKE) --no-print-directory debug-lists

pre-build:
	mkdir -p $(BUILD_DIR)

lists:
	ocamlopt -o $(BUILD_DIR)/$(LISTS_EXEC) $(LISTS_SOURCES)

debug-lists:
	ocamlc -g -o $(LISTS_EXEC) $(LISTS_SOURCES)

tests:
	mkdir -p $(TESTS_DIR)
	ocamlfind ocamlopt -package oUnit -o $(TESTS_DIR)/$(TESTS_EXEC) -linkpkg -g $(LISTS_SOURCES) $(TESTS_SOURCES)

clean:
	rm -f *.cmi *.cmx *.cmo *.o $(LISTS_EXEC)
	rm -rf $(TESTS_DIR)
	rm -rf $(BUILD_DIR)
