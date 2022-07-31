# CEAMEC v1.0

CEAMEC: Cost-Effective Animal Management via Environmental Capacity

## Installation

Please use the latest R: https://www.r-project.org/  

Please use the latest RTools: https://cran.r-project.org/bin/windows/Rtools/

Please use the latest Java (version > 8): https://www.java.com/download

### Install and run CEAMEC by copying the code below to your R console.

```R
list.of.packages <- c("shiny","rgdal","leaflet","shinycssloaders","shinythemes","tibble","unmarked","DT","data.table","xlsx","rgenoud","htmltools","bsplus","dplyr","shinycssloaders","rgeos","plyr","shinyjs")
req.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(req.packages)) install.packages(req.packages, dependencies = TRUE)
shiny::runGitHub('CEAMEC', 'qt37t247')
```


Cloud-based shiny app is available at:
https://qt37t247.shinyapps.io/CEAMEC-master/

## Video tutorial
https://youtu.be/mg-trms15hI


## Input data preparation

Please prepare the input files according to the structure of R package unmarked based on survey types:

Distance sampling: https://cran.r-project.org/web/packages/unmarked/vignettes/distsamp.html

Example data file provided in CEAMEC Github page: distdata.csv and cov.csv



Repeated count: https://studylib.net/doc/6696451/fitting-royle-s-n-mixture-model-with-package-unmarked-in-...

Example data file provided in CEAMEC Github page: mld_pcount.csv



Removal sampling: https://rdrr.io/cran/unmarked/man/ovendata.html

Example data file provided in CEAMEC Github page: oven_removal.csv



Double observer sampling: https://rdrr.io/cran/unmarked/man/unmarkedFrameMPois.html

Example data file provided in CEAMEC Github page: fake_double.csv


In addition, CEAMEC requires a cost file listing unit costs of management methods corresponding to covariates to be managed (see example file "cost.csv")  

## Contact author
Dr. Qian Tang    <<tangbenjamin@hotmail.com>>
