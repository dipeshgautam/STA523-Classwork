# La Quinta files
HW2.html: HW2.Rmd Data/analysis.Rdata Data/plot.png
	Rscript -e "library(rmarkdown);render('HW2.Rmd')"

Data/analysis.Rdata Data/plot.png: distance_analysis.R lq/hotel_list_final.Rdata dennys/dennys_data.Rdata
	Rscript distance_analysis.R

lq/hotel_list_final.Rdata: lq_data_parse2.R lq/lq_URLS.Rdata lq/all_urls.Rdata
	Rscript lq_data_parse2.R

lq/all_urls.Rdata: lq_data_download2.R lq/lq_URLS.Rdata
	Rscript lq_data_download2.R

lq/lq_URLS.Rdata: lq_data_parse.R lq/lq_list.Rdata
	Rscript lq_data_parse.R

lq/lq_list.Rdata: lq_data_download.R
	Rscript lq_data_download.R


# Denny's files

dennys/dennys_data.Rdata: dennys_parse.R dennys/dc.xml dennys/thmb.xml dennys/fbak.xml dennys/slc.xml dennys/hi.xml
	Rscript dennys_parse.R

dennys/dc.xml dennys/thmb.xml dennys/fbak.xml dennys/slc.xml dennys/hi.xml: dennys_download.R
	Rscript dennys_download.R


# Helper


.PHONY: clean
clean:
	rm -rf dennys/
	rm -rf lq/
	rm -f hw2.html