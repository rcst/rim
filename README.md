
|                                                 |
|-------------------------------------------------|
| title: “rmaxima - an interface to maxima for R” |
| output:                                         |
| github\_document:                               |
| md\_extensions: +tex\_math\_single\_backslash   |
| pandoc\_args: \[                                |
| “–mathjax”                                      |
| \]                                              |

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

# Introduction

This section demonstrates the use of `rmaxima` via it’s knitr engine.
Alternatively, you can run each line of the Maxima code chunks via
`maxima.get()`.

Let’s first attach the package. This will start Maxima automatically and
set the output format to LaTeX, which is practical for the purpose of
this page.

``` r
library(rmaxima)
```

Now we can enter Maxima expression inside code chunks. Note that we are
not required to end each line by `;` or `$`. If omitted a `;` is
inserted automatically.

``` maxima
L: sqrt(1 - 1/R^2);
assume(R > 0);
integrate(x, x, 0, L);
```

$$\\sqrt{1-\\frac{1}{R^2}}$$
\[*R*&gt;0\]
$$\\frac{R^2-1}{2\\,R^2}$$

`knitr` prints the results as verbatim code (as usual). We for a
different more readable output, we can simply add the chunk option
`results = 'asis'`:

<math> <msqrt><mn>1</mn> <m
o>-</mo> <mfrac><mrow><mn>1</mn> </mrow> <mrow><msup><mrow><mi>R</mi>
</mrow> <mn>2</mn> </msup> </mrow></mfrac> </msqrt></math>

\\\[ \\sqrt{1-\\frac{1}{R^2}} \\\]
