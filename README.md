
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mastermapr

<!-- badges: start -->

<!-- badges: end -->

The goal of mastermapr is to make it easier to import data from Ordnance
Survey’s MasterMap products into
R.

## Installation

<!-- You can install the released version of mastermapr from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("mastermapr") -->

<!-- ``` -->

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("itsleeds/mastermapr")
```

## Reading-in mastermap data

MasterMap products come in a zip file. Once unzipped you can read-in
MasterMap files as follows (you need to apply for a license for many of
their products):

``` r
library(mastermapr)
## basic example code
mm_highway = read_mastermap("~/hd/data/os/Download_mastermap-roads-2019_1483661/MasterMap Highways Network_rami_3480319/", n = 3)
#> Found these files (first 3 of 3 to read): Highways_Rrami_RoadLink_FULL_001.gml.gzHighways_Rrami_RoadLink_FULL_002.gml.gzHighways_Rrami_RoadLink_FULL_003.gml.gz
plot(mm_highway["roadClassification"])
```

<img src="man/figures/README-example-1.png" width="100%" />

## Next steps

So far this package has limited functionality but it may be expanded in
the future. Watch this space and feel free to get involved via the issue
tracker if interested\!
