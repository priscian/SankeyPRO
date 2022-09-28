# SankeyPRO
Create Sankey diagrams from patient-recorded outcomes (PRO) data or other data

## Introduction
The visualization of patient-recorded outcomes ([PRO](https://en.wikipedia.org/wiki/Patient-reported_outcome)) over time can provide preliminary evaluations of temporal trends. One visualization, the [Sankey diagram](https://en.wikipedia.org/wiki/Alluvial_diagram), nicely shows both the frequency and redistribution of a PRO's categories from time point to time point.

The R package *SankeyPRO* readily allows the alluvial visualization of a single PRO variable collected at multiple time points.

## Preliminaries
The *SankeyPRO* R package is fairly easy to set up. In an R session:
```
install.packages("remotes") # If necessary.
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true") # https://github.com/r-lib/remotes#environment-variables
remotes::install_github("priscian/SankeyPRO")
library(SankeyPRO)

## Once the package has been installed as described above, all you need to use it is:
library(SankeyPRO)
```

## Using *SankeyPRO*
