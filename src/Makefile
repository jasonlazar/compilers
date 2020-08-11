CXXFLAGS=-g -Wall
CXX=c++

.PHONY: default clean distclean

default: tony

lexer.cpp: lexer.l
	flex -s -o $@ $<

lexer.o: lexer.cpp lexer.hpp

tony: lexer.o
	$(CXX) $(CXXFLAGS) -o $@ $^ -lfl

clean:
	$(RM) lexer.cpp *.o *~

distclean: clean
	$(RM) tony$(EXE)
