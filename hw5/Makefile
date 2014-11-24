## This Makefile is used to generate the final document

hw5.html: hw5.Rmd table.Rdata
	Rscript -e "library(rmarkdown);render('hw5.Rmd')"

table.Rdata: create_table.R
	Rscript create_table.R

## Helper

.PHONY: clean
clean:
	rm -f hw5.html