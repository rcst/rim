rim - R’s interface to Maxima
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

Development version of the yet-to-be-released CRAN package. rim provides
an interface to the powerful and fairly complete maxima computer algebra
system

This repository is a fork from RMaxima, the original version created by
Kseniia Shumelchyk and Hans W. Borcher
[shumelchyk/rmaxima](https://github.com/shumelchyk/rmaxima), which is
currently not maintained.

# Installation

## Requirements

-   Maxima

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
install.packages("rim")
```

# Usage

This section only demonstrate using the packages R-function directly
accessible to the user. On how to use the package’s `knitr` engine see
[this page](https://rcst.github.io/rim/).

``` r
library(rim)
```

``` r
maxima.start(restart = TRUE)
```

    ## trying port 27182

``` r
maxima.get("1+1;")
```

    ## [1] "(%o1) 2"

``` r
maxima.get("y: 0.5 * t^2 * g + g*t;")
```

    ## [1] "(%o2) 0.5*g*t^2+g*t"

``` r
maxima.get("diff(y, t);")
```

    ## [1] "(%o3) 1.0*g*t+g"

``` r
maxima.stop()
```
