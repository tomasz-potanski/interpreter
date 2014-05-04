all:
	happy -gca Paractualgrammar.y
	alex -g Lexactualgrammar.x
	ghc --make Testactualgrammar.hs -o verifier
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docactualgrammar.ps
distclean: clean
	-rm -f Docactualgrammar.* Lexactualgrammar.* Paractualgrammar.* Layoutactualgrammar.* Skelactualgrammar.* Printactualgrammar.* Testactualgrammar.* Absactualgrammar.* Testactualgrammar ErrM.* SharedString.* actualgrammar.dtd XMLactualgrammar.* Makefile*

