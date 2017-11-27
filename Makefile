
CMULISP = lisp

all: compile

compile:
	$(CMULISP) -eval '(progn (mk:compile-system :l-lisp) (ext:quit))'

clean:
	rm -f *.x86f *.err *~
