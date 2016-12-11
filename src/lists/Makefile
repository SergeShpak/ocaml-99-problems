BUILD_DIR = build
LISTS_SOURCES = lists.mli lists.ml
LISTS_EXEC = lists
TESTS_SOURCES = tests.ml
TESTS_EXEC = tests
TESTS_DIR = tests

all: post-build 

post-build: main-build

main-build: pre-build
	@$(MAKE) --no-print-directory lists

pre-build:
	mkdir -p $(BUILD_DIR)

lists:
	ocamlopt -o $(BUILD_DIR)/$(LISTS_EXEC) $(LIST_SOURCES)

tests:
	mkdir -p $(TESTS_DIR)
	ocamlfind ocamlopt -package oUnit -o $(TESTS_DIR)/$(TESTS_EXEC) -linkpkg -g $(LISTS_SOURCES) $(TESTS_SOURCES)

clean:
	rm -f *.cmi *.cmx *.o $(EXEC)
	rm -rf $(TESTS_DIR)
	rm -rf $(BUILD_DIR)
