pdf :=\
  scheme-coding-standard.pdf\
  swish-lite.pdf\

.SUFFIXES:

.PHONY: all
all: $(pdf)

%.pdf: %.tex sagian.sty swish.sty reference.bib
	./run-latex $*

swish :=\
  erlang.tex\
  html.tex\
  json.tex\
  pregexp.tex\
  heap.tex\
  oop.tex\
  stream.tex\
  cli.tex\
  testing.tex\

swish-lite.pdf: $(swish) oop-layout.pdf

oop-layout.pdf: oop-layout.dot
	dot -Tpdf -o $@ $<

.PHONY: clean
clean:
	rm -f $(pdf) oop-layout.pdf *.aux *.bbl *.blg *.lof *.log *.lot *.out *.idx *.ind *.ilg *.toc *.thm
