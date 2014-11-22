## This Makefile is used to generate the final document

hw5.html: hw5.Rmd Data/analysis.Rdata Data/plot.png
  Rscript -e "library(rmarkdown);render('hw5.Rmd')"

## 

Data/analysis.Rdata Data/plot.png: 
distance_analysis.R lq/hotel_list_final.Rdata dennys/dennys_data.Rdata
	Rscript reject.R

## Helper

.PHONY: clean
clean:
rm -rf dennys/
rm -rf lq/
rm -f hw5.html