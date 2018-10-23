all: install

install:
	ghc main.hs

clean:
	rm main main.hi main.o
