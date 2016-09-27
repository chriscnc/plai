
all: 
	raco make parser.rkt

test:
	raco test parser-test.rkt

clean:
	rm -rf compiled
