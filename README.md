# SankeyPRO
Create Sankey diagrams from patient-recorded outcomes (PRO) data or other data

## Introduction
The visualization of patient-recorded outcomes ([PRO](https://en.wikipedia.org/wiki/Patient-reported_outcome)) over time can provide preliminary evaluations of temporal trends. One visualization, the [Sankey diagram](https://en.wikipedia.org/wiki/Alluvial_diagram), nicely shows both the frequency and redistribution of a PRO's categories from time point to time point.

The R package *SankeyPRO* readily allows the alluvial visualization of a single PRO variable collected at multiple time points.

## Preliminaries
The *SankeyPRO* R package is fairly easy to set up. In an R session:
```
install.packages("remotes") # If necessary.
## https://github.com/r-lib/remotes#environment-variables
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
remotes::install_github("priscian/SankeyPRO")
library(SankeyPRO)

## Once the package has been installed as described above, all you need to use it is:
library(SankeyPRO)
```

## Using *SankeyPRO*
The input PRO data should be in so-called long format, with a single entry for every subject for every time point, e.g.

```
> pro
# A tibble: 500 × 3
      id  time   pro
   <dbl> <int> <int>
 1   547     1     0
 2   547     2     1
 3   547     3     3
 4   658     1     1
 5   385     1     1
 6   385     2     2
 7   385     3     1
 8   385     4     1
 9   202     1     2
10   202     2     2
# … with 490 more rows
```

You can then produce a Sankey diagram by calling the function `plot_sankeypro()` and relying mostly on its default arguments:

```

```
