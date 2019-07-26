### Running with recordr

There are a couple caveats with recorder. 

1. Installing it view `install.packages` is not supported in the latest R version (so we manually build it)
2. It doesn't track rmarkdown output

```
cd recordr

git clone https://github.com/NCEAS/recordr.git

R CMD build recordr

R CMD INSTALL recordr_1.0.3.9000.tar.gz

Rscript record-run.R

```

#### Output
