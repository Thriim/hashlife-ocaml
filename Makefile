
EXECS = hashlife main.byte main.native testRapport

all:
	ocp-build
	mv _obuild/hashlife-gtk/hashlife-gtk.byte hashlife

clean:
	rm -f *.cm[iox] *~ .*~ *.o #*#
	rm -f $(EXECS)
	ocp-build clean
