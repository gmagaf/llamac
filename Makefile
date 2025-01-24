.PHONY: clean

Lexer.hs:
	alex -g --latin1 -o Lexer.hs lexer/Lexer.x

clean:
	$(RM) Lexer.hs *.o *.hi
