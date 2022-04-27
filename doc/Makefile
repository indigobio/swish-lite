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
  json.tex\
  pregexp.tex\
  heap.tex\
  stream.tex\
  cli.tex\
  testing.tex\

swish-lite.pdf: $(swish)

.PHONY: clean
clean:
	rm -f $(pdf) *.aux *.bbl *.blg *.lof *.log *.lot *.out *.idx *.ind *.ilg *.toc *.thm
