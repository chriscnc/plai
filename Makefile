
all: 
	raco make interp.rkt

test:
	raco test interp-test.rkt

clean:
	rm -rf compiled
	rm *~
