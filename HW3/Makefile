# Final Output
HW3.html: HW3.Rmd precinct.json plot.png
	Rscript -e "library(rmarkdown);render('HW3.Rmd')"

precinct.json plot.png: recreate_police_precincts.R manh.RData
	Rscript recreate_police_precincts.R

manh.RData: geocode.R
	Rscript geocode.R

# Helper


.PHONY: clean
clean:
	rm -f precinct.json
	rm -r plot.png
	rm -f manh.RData
	rm -f HW3.html
