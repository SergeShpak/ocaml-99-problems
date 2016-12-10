SOURCES = lists.mli lists.ml
EXEC = lists
TESTS_SOURCES = tests.ml
TESTS_EXEC = tests
TESTS_DIR = tests

all:
	ocamlopt -o $(EXEC) $(SOURCES)

tests:
	mkdir -p $(TESTS_DIR)
	ocamlfind ocamlopt -package oUnit -o $(TESTS_DIR)/$(TESTS_EXEC) -linkpkg -g $(SOURCES) $(TESTS_SOURCES)

clean:
	rm -f *.cmi *.cmx *.o $(EXEC)
	rm -rf $(TESTS_DIR)
