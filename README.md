# CEAMEC v1.0
Cost-Effective Animal Management via Environmental Capacity

## Installation

```R
install.packages("shiny")
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
