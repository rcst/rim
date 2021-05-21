rmaxima - an interface to maxima for R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

Development version of the yet-to-be-released rmaxima CRAN package.
rmaxima provides an interface to the powerful and fairly complete maxima
computer algebra system

This repository is a fork from the original version created by Kseniia
Shumelchyk and Hans W. Borcher
[shumelchyk/rmaxima](https://github.com/shumelchyk/rmaxima), which is
currently not maintained.

# Installation

## Requirements

-   this package currently only works under Linux (and has not been
    tested under MacOS)
-   you can install it without having Maxima installed, but obviously
    need to install Maxima in order to use it.

## Steps

If you want to install the latest version (currently the only one
available) install the R package `drat` first and add this github
account as a repo:

``` r
install.packages("drat")
drat::addRepo("rcst")
```

Now you can easily install it the usual way:

``` r
install.packages("rmaxima")
```

# Usage

This section only demonstrate using the packages R-function directly
accessible to the user. On how to use the package’s `knitr` engine see
[this page](https://rcst.github.io/rmaxima).

``` r
library(rmaxima)
```

``` r
maxima.start(restart = TRUE)
maxima.get("1+1")
```

    ## [1] "2"
    ## attr(,"input.label")
    ## [1] "%i1"
    ## attr(,"output.label")
    ## [1] "%o1"
    ## attr(,"command")
    ## [1] "1+1"
    ## attr(,"format")
    ## [1] "linear"

``` r
maxima.stop()
```
