all: vignettes move clean

vignettes:
	Rscript -e "setwd('source');library('rmarkdown');files=dir();for(file in files) render(file)"

move:
	mv -f source/*.html html/

clean:
	$(RM) -r source/*_cache/;
	$(RM) -r source/*_files/;
	$(RM) -r source/figure/;
	$(RM) -r source/SimDesign*/;
	$(RM) -r html/reg.html;
	$(RM) source/*.md;
	$(RM) source/*.txt;

