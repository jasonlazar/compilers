CXXFLAGS=-g -Wall -std=c++11 `$(LLVMCONFIG) --cxxflags`
LLVMCONFIG=llvm-config-9
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`
CXX=c++
# CXX=clang++-9
CFLAGS=-g

.PHONY: default clean distclean

default: tony

lexer.cpp: lexer.l
	flex -s -o $@ $<

lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

parser.o: parser.cpp lexer.hpp ast.hpp

tony: lexer.o parser.o printers.o library.o symbol.o general.o error.o ast.o
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o

distclean: clean
	$(RM) tony
