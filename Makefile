COMPILER=ocamlopt
ARGS=-unsafe

all: bin/slide_puzzle bin/vacuum bin/labirinth bin/eight_queens

bin/slide_puzzle: bin tools.ml graph.ml slide_puzzle.ml
	$(COMPILER) $(ARGS) -o bin/slide_puzzle tools.ml graph.ml slide_puzzle.ml

bin/vacuum: bin tools.ml graph.ml vacuum.ml
	$(COMPILER) $(ARGS) -o bin/vacuum tools.ml graph.ml vacuum.ml

bin/labirinth: bin local_search.ml labirinth.ml
	$(COMPILER) $(ARGS) -o bin/labirinth local_search.ml labirinth.ml

bin/eight_queens: bin tools.ml local_search.ml eight_queens.ml
	$(COMPILER) $(ARGS) -o bin/eight_queens tools.ml local_search.ml eight_queens.ml

bin:
	mkdir bin

clean:
	'rm' -f *.o
	'rm' -f *.cm*
	'rm' -fr bin

