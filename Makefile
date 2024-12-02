# Commands:

build: 
	ghc --make -O -o chessGPT Main.hs

prof:
	ghc --make -prof -o chessGPT Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f chessGPT
	rm -f *.hi
	rm -f *.o