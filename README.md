
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `RFCLUST`

<!-- badges: start -->

[![R-CMD-check](https://github.com/sistm/RFCLUST/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sistm/RFCLUST/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`RFCLUST` performs Random Forests of Divisive Monothetic
([`divclust`](https://github.com/chavent/divclust)) Trees for
Unsupervised Clustering.

## Installation

You can install the development version of RFCLUST from
[GitHub](https://github.com/).

`RFCLUST`depends on a custopmized implementation of the
[`divclust`](https://github.com/chavent/divclust) package, that must
first be installed with the following:

``` r
# install.packages("remotes")
remotes::install_github("sistm/divclust")
```

Then, `RFCLUST` can be installed with:

``` r
remotes::install_github("sistm/RFCLUST")
```

## Example

``` r
library(RFCLUST)
#> Loading required package: parallel
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
library(palmerpenguins)
mypeng <- as.data.frame(penguins)
mypeng$year <- factor(as.character(mypeng$year),
                         levels=c("2007", "2008", "2009"),
                         ordered=TRUE)

forest_clust <- rfclust(na.omit(mypeng[mypeng$sex=="male", -c(1, 7)]), ntrees = 50, ncores = 1)
#> We advise you do use the `summary()` on this object to agregate the result of this forest, before plotting the summary itself.

resume <- summary(forest_clust)
plot(resume)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
