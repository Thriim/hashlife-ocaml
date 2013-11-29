
EXECS = hashlife main.byte main.native testRapport

all:
	ocamlbuild -pkgs cairo,lablgtk2,lablgtk2.auto-init,cairo.lablgtk2 main.byte
	mv main.byte hashlife

testRapport:
	ocamlbuild -pkgs cairo,lablgtk2,lablgtk2.auto-init,cairo.lablgtk2 testRapport.byte
	mv testRapport.byte testRapport

clean:
	rm -f *.cm[iox] *~ .*~ *.o #*#
	rm -f $(EXECS)
