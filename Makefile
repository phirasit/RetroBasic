SOURCE_OUT=main
CHECK_OUT=check

all: install

install:
	gcc lister/*.c lister/*.h -o check
	ghc main.hs

clean:
	rm *.hi *.o
	rm main check
