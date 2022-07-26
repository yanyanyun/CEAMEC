# CEAMEC v1.0
Cost-Effective Animal Management via Environmental Capacity

Cloud-based shiny app is available at:
https://qt37t247.shinyapps.io/CEAMEC-master/

## Demo-videos
Running in Shiny Cloud:
https://youtu.be/_wW5g42nHcg

Running in RStudio
https://youtu.be/mg-trms15hI

## Installation

Please update your R and RTools (RStudio) to the latest version
R: https://www.r-project.org/
RTools: https://cran.r-project.org/bin/windows/Rtools/
RStudio: https://www.rstudio.com/products/rstudio/download/

It is required to install/update your Java (version > 8)
https://www.java.com/download

```R
list.of.packages <- c("shiny","rgdal","leaflet","shinycssloaders","shinythemes","tibble","unmarked","DT","data.table","xlsx","rgenoud","htmltools","bsplus","dplyr","shinycssloaders","rgeos","plyr")
req.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(req.packages)) install.packages(req.packages, dependencies = TRUE)
shiny::runGitHub('CEAMEC', 'qt37t247')
```

## Input data preparation

Before using CEAMEC, please prepare the input files according to the structure of R package unmarked:


Distance sampling:
Example data file provided in CEAMEC Github page: distdata.csv and cov.csv

https://rdrr.io/cran/unmarked/f/inst/doc/distsamp.pdf

https://rdrr.io/cran/unmarked/man/unmarkedFrameDS.html


Repeated count:
Example data file provided in CEAMEC Github page: mld_pcount.csv

https://rdrr.io/cran/unmarked/man/unmarkedFramePCount.html

https://studylib.net/doc/6696451/fitting-royle-s-n-mixture-model-with-package-unmarked-in-...


Removal sampling:
Example data file provided in CEAMEC Github page: oven_removal.csv

https://rdrr.io/cran/unmarked/man/ovendata.html


Double observer sampling:
Example data file provided in CEAMEC Github page: fake_double.csv

https://rdrr.io/cran/unmarked/man/unmarkedFrameMPois.html


In addition, CEAMEC requires a cost file listing unit costs of management methods corresponding to covariates to be managed (see example file "cost.csv")  

## Contact author

tangqiannus@gmail.com
