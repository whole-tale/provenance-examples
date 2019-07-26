Rscript install.R
Rscript smallest_k_sim.R
R -e "rmarkdown::render('smallest_k_explainer.Rmd', output_dir = 'results/')"