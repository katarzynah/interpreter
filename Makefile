all:
	happy -gca ParInterpreter.y
	alex -g LexInterpreter.x
	ghc --make Interpret.hs -o Interpret

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocInterpreter.* LexInterpreter.* ParInterpreter.* LayoutInterpreter.* SkelInterpreter.* PrintInterpreter.* TestInterpreter.* AbsInterpreter.* TestInterpreter ErrM.* SharedString.* ComposOp.* Interpreter.dtd XMLInterpreter.* Makefile*
	

