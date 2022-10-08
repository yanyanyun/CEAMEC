# CEAMEC v1.0

CEAMEC: Cost-Effective Animal Management via Environmental Capacity

## Web-based quick access

For those do not want to be troubled with R and have small projects, please feel free to access CEAMEC on the Shiny Cloud:
https://qt37t247.shinyapps.io/ceamec/

## Installation

* Please use the latest R: https://www.r-project.org/  

* Please use the latest RTools: https://cran.r-project.org/bin/windows/Rtools/

* Please use the latest Java (version > 8): https://www.java.com/download

### Install and run CEAMEC by copying the code below to your R console.

Check whether all dependent R packages are installed (for the first time installing CEAMEC).
```R
list.of.packages <- c("shiny","rgdal","leaflet","shinycssloaders","shinythemes","tibble","unmarked","DT","data.table","xlsx","rgenoud","htmltools","bsplus","dplyr","shinycssloaders","rgeos","plyr","shinyjs")
req.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(req.packages)) install.packages(req.packages, dependencies = TRUE)
```

Install and run CEAMEC.
```R
shiny::runGitHub('CEAMEC', 'qt37t247')
```

## Video tutorial
https://youtu.be/gWALmOzZHJc


## Input data preparation

Please prepare the input files according to the structure of R package unmarked based on survey types:

* Distance sampling: https://cran.r-project.org/web/packages/unmarked/vignettes/distsamp.html

* Repeated count: https://studylib.net/doc/6696451/fitting-royle-s-n-mixture-model-with-package-unmarked-in-...

* Removal sampling: https://rdrr.io/cran/unmarked/man/ovendata.html

* Double observer sampling: https://rdrr.io/cran/unmarked/man/unmarkedFrameMPois.html

## Contact author
Dr. Qian Tang    <<tangbenjamin@hotmail.com>>
