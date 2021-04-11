
ARGS=test.zip test1.txt 

# all: dune build oczip.ml

all:
	@dune build

run:
	@dune exec -- ./oczip.exe $(ARGS)

test:
	@dune test

clean: 
	rm test.zip

