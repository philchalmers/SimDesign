all: vignettes move clean commit push

vignettes:
	Rscript -e "setwd('source');files=dir();files=files[grepl('.[Qq]md',files)];for(file in files) system(sprintf('quarto render %s',file))"

script2qmd:
	Rscript -e "setwd('source_solve');files=dir();for(file in files) knitr::spin(file, format='qmd', knit=FALSE)"
	mv -f source_solve/*.qmd source/

extra:
	Rscript -e "setwd('extras');dirs=dir();for(d in dirs){setwd(d);files <- dir();source(files[which.min(nchar(files))]); setwd('..')}"

move:
	mv -f source/*.html html/

clean:
	$(RM) -r source/*_cache/;
	$(RM) -r source/*_files/;
	$(RM) -r source/figure/;
	$(RM) -r source/SimDesign*/;
	$(RM) source/SimSolve_*.qmd;
	$(RM) -r html/reg.html;
	$(RM) source/*.md;
	$(RM) source/*.txt;
	$(RM) source/*.html;

commit:
	git commit -am "update"

push:
	git pull --rebase
	git push
