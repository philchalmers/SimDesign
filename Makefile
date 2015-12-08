all: vignettes move clean

vignettes:
	Rscript -e "setwd('source');library('knitr');files=dir();for(file in files) knit2html(file)"	

move:
	mv -f source/*.html html/

clean:
	$(RM) -r source/cache/;
	$(RM) -r source/figure/;
	$(RM) source/*.md;
	$(RM) source/*.txt;

