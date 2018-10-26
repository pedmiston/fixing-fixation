fixing-fixation.pdf: manuscript.Rmd analyses.R
%.pdf: %.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
