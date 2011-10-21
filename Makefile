DOC=fp-intro

all:
  # pdflatex $(DOC).tex
  # bibtex $(DOC)
	pdflatex $(DOC).tex
	pdflatex $(DOC).tex

clean:
	rm -f *~ *.bbl *.blg *.log *.aux *.out *.snm *.toc *.nav *.synctex.gz 
	rm -f $(DOC).pdf
