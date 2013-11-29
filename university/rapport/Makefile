
rapport.pdf: rapport.tex ocamldoc.out
	rubber -d rapport.tex

ocamldoc.out: ../mcell.mli
	ocamldoc -latex -noheader -notrailer ../mcell.mli -o $@

clean:
	rm -f *~ \#*\# *.aux *.log


