CXXFLAGS=-g -Wall
CXX=c++

.PHONY: clean distclean

minibasic: lexer.o
	$(CXX) $(CXXFLAGS) -o $@ $^ -lfl

lexer.cpp: lexer.l
	flex -s -o $@ $<

clean:
	$(RM) lexer.cpp *.o *~

distclean: clean
	$(RM) minibasic$(EXE)
