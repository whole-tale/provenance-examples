Rscript 'install.R'
Rscript 'wrangle.R'
R -e "rmarkdown::render('sandbox/EDA.Rmd', output_dir= 'results/eda/')" 
R -e "rmarkdown::render('index.Rmd', output_dir = 'results/')"
