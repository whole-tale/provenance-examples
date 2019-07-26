## A test for density dependence in monocultures   of native California annual seeds.

### Structure

`experiment/`: Holds the R scripts, data, and driver

`with-driver`: Holds output from a reprozip run that used the `run.sh` driver

`without-driver`: Holds output from a reprozip run that didn't use the `run.sh` driver

### What the experiment does

The experiment runs the single `Rmd`, which reads `data/phytometer_data_trials_2018.csv`. It uses `phacelia.jpg` when rendering `results/index.html`.

### Running with Sciunit

```
sciunit open Density-Dependence-in-Annual-Plant-Species-in-the-San-Joaquin-Desert.zip

sciunit repeat e1

```

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
```
[1] "urn:uuid:a5f52b1d-15c8-468a-b4d3-79938f3cc127"
Seq   Script                         Tag                  Start Time
1     /provenance-e.../recordr/run.R Density Experiment   2019-07-26 18:59:08 UTC

[details]: Run details
----------------------
"/provenance-examples/examples/Den...e-San-Joaquin-Desert/recordr/run.R" was executed on 2019-07-26 18:59:08 UTC
Tag: "Density Experiment"
Run sequence #: 1
Publish date: Not published
Published to: NA
Published Id: NA
View at: NA
Run by user: root
Account subject: NA
Run Id: urn:uuid:a5f52b1d-15c8-468a-b4d3-79938f3cc127
Data package Id: urn:uuid:55488570-4d20-4186-b3a8-fc80c6b3018d
HostId: 078cd927478f
Operating system: x86_64-pc-linux-gnu
R version: R version 3.6.0 (2019-04-26)
Dependencies: stats, graphics, grDevices, utils, datasets, methods, base, Rcpp_1.0.1, pillar_1.4.2, compiler_3.6.0, tools_3.6.0, emld_0.2.0, digest_0.6.20, uuid_0.1-2, bit_1.1-14, jsonlite_1.6, evaluate_0.14, RSQLite_2.1.1, memoise_1.1.0, tibble_2.1.3, pkgconfig_2.0.2, rlang_0.4.0, DBI_1.0.0, commonmark_1.7, datapack_1.3.1, curl_3.3, yaml_2.2.0, EML_2.0.0, xfun_0.8, stringr_1.4.0, roxygen2_6.1.1, jqr_1.1.0, dplyr_0.8.3, xml2_1.2.0, knitr_1.23, rappdirs_0.3.1, redland_1.0.17-10, jsonld_2.1, bit64_0.9-7, tidyselect_0.2.5, glue_1.3.1, R6_2.4.0, hash_2.2.6.1, XML_3.98-1.20, rmarkdown_1.13, purrr_0.3.2, blob_1.1.1, magrittr_1.5, htmltools_0.3.6, assertthat_0.2.1, V8_2.3, stringi_1.4.3, lazyeval_0.2.2, crayon_1.3.4, recordr_1.0.3.9000
Run start time: 2019-07-26 18:59:08 UTC
Run end time: 2019-07-26 18:59:13 UTC
Error message from this run: NA

[used]: 1 items used by this run
-----------------------------------
Location                                                     Size (kb)    Modified time
/provenance-examples/example...ytometer_data_trials_2018.csv 132370       2019-07-25 19:42:00
```


### Graphs

With Driver:

<img src="with-driver/graph.svg">

Without Driver:

<img src="without-driver/graph.svg">
