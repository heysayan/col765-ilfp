# Makefile for the Prolog-like Resolution Engine
# Builds the interpreter with Ocamllex and Ocamlyacc

# Compiler and flags
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLFLAGS = -w -a

# Source files
LEXER_SRC = lexer.mll
PARSER_SRC = parser.mly
MAIN_SRC = assgn9.ml

# Generated files
LEXER_ML = lexer.ml
PARSER_ML = parser.ml
PARSER_MLI = parser.mli

# Object files
PARSER_CMO = parser.cmo
LEXER_CMO = lexer.cmo
MAIN_CMO = assgn9.cmo

# Executables
BYTECODE = prolog_engine
NATIVE = prolog_engine.opt

# Default target
.PHONY: all
all: bytecode

# Bytecode executable
.PHONY: bytecode
bytecode: $(BYTECODE)

$(BYTECODE): $(PARSER_CMO) $(LEXER_CMO) $(MAIN_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $^

# Native executable (optional, faster)
.PHONY: native
native: $(NATIVE)

$(NATIVE): parser.cmx lexer.cmx assgn9.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ $^

# Generate parser from .mly
$(PARSER_ML) $(PARSER_MLI): $(PARSER_SRC)
	$(OCAMLYACC) $<

# Generate lexer from .mll
$(LEXER_ML): $(LEXER_SRC) $(PARSER_MLI)
	$(OCAMLLEX) $<

# Compile parser
parser.cmo: $(PARSER_ML) $(PARSER_MLI)
	$(OCAMLC) $(OCAMLFLAGS) -c $(PARSER_MLI)
	$(OCAMLC) $(OCAMLFLAGS) -c $(PARSER_ML)

parser.cmx: $(PARSER_ML) $(PARSER_MLI)
	$(OCAMLOPT) $(OCAMLFLAGS) -c $(PARSER_MLI)
	$(OCAMLOPT) $(OCAMLFLAGS) -c $(PARSER_ML)

# Compile lexer
lexer.cmo: $(LEXER_ML) $(PARSER_MLI)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

lexer.cmx: $(LEXER_ML) $(PARSER_MLI)
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

# Compile main
assgn9.cmo: $(MAIN_SRC) $(PARSER_MLI)
	$(OCAMLC) $(OCAMLFLAGS) -c $<

assgn9.cmx: $(MAIN_SRC) $(PARSER_MLI)
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

# Test with example file
.PHONY: test
test: bytecode
	./$(BYTECODE) test_program.pl

# Clean generated files
.PHONY: clean
clean:
	rm -f $(LEXER_ML) $(PARSER_ML) $(PARSER_MLI)
	rm -f *.cmo *.cmi *.cmx *.o
	rm -f $(BYTECODE) $(NATIVE)

# Clean everything including backups
.PHONY: distclean
distclean: clean
	rm -f *~ *.bak

# Help
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all        - Build bytecode executable (default)"
	@echo "  bytecode   - Build bytecode executable"
	@echo "  native     - Build native code executable (faster)"
	@echo "  test       - Run test program"
	@echo "  clean      - Remove generated files"
	@echo "  distclean  - Remove all generated and backup files"
	@echo "  help       - Show this help message"
	@echo ""
	@echo "Usage:"
	@echo "  ./$(BYTECODE)                - Start interactive REPL"
	@echo "  ./$(BYTECODE) <file.pl>      - Run program from file"
