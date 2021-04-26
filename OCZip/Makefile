
ARGS=-zip -o test.zip test-files/blah.txt

# all: dune build oczip.ml

all:
	@dune build

run:
	dune exec -- ./oczip.exe $(ARGS)

stress: ARGS=-zip -o stress.zip test-files/blah.txt test-files/bnj.txt test-files/hw.txt test-files/stay-large.png test-files/to-small.jpg
stress: run

test:
	@dune test

clean: 
	rm test.zip stress.zip

