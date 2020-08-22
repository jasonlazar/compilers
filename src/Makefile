CXXFLAGS=-g -Wall
CXX=c++

.PHONY: default clean distclean

default: tony

lexer.cpp: lexer.l
	flex -s -o $@ $<

lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

parser.o: parser.cpp lexer.hpp ast.hpp

tony: lexer.o parser.o printers.o symbol.o general.o error.o
	$(CXX) $(CXXFLAGS) -o $@ $^

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o

distclean: clean
	$(RM) tony
