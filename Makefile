COMPILER=ocamlopt
ARGS=-unsafe -O3

all: bin/slide_puzzle bin/vacuum bin/labirinth bin/eight_queens bin/tic_tac_toe

bin/slide_puzzle: bin tools.ml graph.ml slide_puzzle.ml
	$(COMPILER) $(ARGS) -I +unix -o bin/slide_puzzle unix.cmxa tools.ml graph.ml slide_puzzle.ml

bin/vacuum: bin tools.ml graph.ml vacuum.ml
	$(COMPILER) $(ARGS) -o bin/vacuum tools.ml graph.ml vacuum.ml

bin/labirinth: bin tools.ml local_search.ml labirinth.ml
	$(COMPILER) $(ARGS) -o bin/labirinth tools.ml local_search.ml labirinth.ml

bin/eight_queens: bin tools.ml local_search.ml eight_queens.ml
	$(COMPILER) $(ARGS) -I +unix -o bin/eight_queens unix.cmxa tools.ml local_search.ml eight_queens.ml

bin/tic_tac_toe: bin tools.ml games.ml tic_tac_toe.ml
	$(COMPILER) $(ARGS) -o bin/tic_tac_toe tools.ml games.ml tic_tac_toe.ml

bin:
	mkdir bin

clean:
	'rm' -f *.o
	'rm' -f *.cm*
	'rm' -fr bin

