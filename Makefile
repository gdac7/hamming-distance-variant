build:
	ghc -o main.exe Main.hs

# Run
run: build
	./main.exe