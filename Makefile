COMPILER=ocamlopt
ARGS=-unsafe

all: bin/slide_puzzle

bin/slide_puzzle: slide_puzzle.ml graph.ml
	$(COMPILER) $(ARGS) -o bin/slide_puzzle graph.ml slide_puzzle.ml

