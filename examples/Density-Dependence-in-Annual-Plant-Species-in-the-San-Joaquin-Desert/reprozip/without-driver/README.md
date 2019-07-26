### No Run.sh
This is an example where `install.R` was run outside of reprozip, and `run.sh` was not used.


#### Instructions

```
Rscript install.R

reprozip trace R -e "rmarkdown::render('index.Rmd', output_dir = 'results/')"

reprozip pack Density-Dependence

reprounzip graph --processes process --packages drop --otherfiles io --regex-filter ^/etc graphfile.dot Density-Dependence.rpz

dot -Tsvg graphfile.dot -o graph.svg

```


#### Graph
<img src="./graph.svg">
