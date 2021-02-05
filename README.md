# CEAMEC v1.0
Cost-Effective Animal Management via Environmental Capacity

## Installation

```R
install.package("shiny")
shiny::runGitHub('CEAMEC', 'yanyanyun')
```

## Input data preparation

CEAMEC requires users to upload two files(in .csv format):

A density file of modified output from the results of hierarchical modelling with R package [unmarked](https://cran.r-project.org/web/packages/unmarked/) (see example file "density_CEAMEC.csv") 

A cost file listing density model coefficients and unit costs of items to be managed (see example file "cost_CEAMEC.csv")  

## Contact author

yanyanyun1128@gmail.com
