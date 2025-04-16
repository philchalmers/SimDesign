all: script2qmd vignettes move clean clean_solve commit push

vignettes:
	Rscript -e "setwd('source');library('rmarkdown');files=dir();for(file in files) render(file)"

script2qmd:
	Rscript -e "setwd('source_solve');files=dir();for(file in files) knitr::spin(file, format='qmd', knit=FALSE)"
	mv -f source_solve/*.qmd source/

extra:
	Rscript -e "setwd('extras');dirs=dir();for(d in dirs){setwd(d);files <- dir();source(files[which.min(nchar(files))]); setwd('..')}"

move:
	mv -f source/*.html html/
	mv -f source_solve/*.html html/

clean:
	$(RM) -r source/*_cache/;
	$(RM) -r source/*_files/;
	$(RM) -r source/figure/;
	$(RM) -r source/SimDesign*/;
	$(RM) -r html/reg.html;
	$(RM) source/*.md;
	$(RM) source/*.txt;
	$(RM) source/*.html;

clean_solve:
	$(RM) -r source_solve/*_cache/;
	$(RM) -r source_solve/*_files/;
	$(RM) -r source_solve/figure/;
	$(RM) -r source_solve/SimDesign*/;
	$(RM) source_solve/*.md;
	$(RM) source_solve/*.txt;
	$(RM) source_solve/*.html;

commit:
	git commit -am "update"

push:
	git push
