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

-   Maxima
-   Boost library (&gt;=1.67.0)

# TODO

## General

-   implement knitr-engine for maxima using this interface
-   Implement stop function to end maxima child process for debugging
    purposes
-   dealing with documentation/ help functions
    -   write corresponding class functions
    -   Help functions in maxima:
        -   `example(...)`
            -   example can be executed, but maxima.tell() only returns
                the last output
            -   implement function `example()` that returns multiple
                outputs
            -   implement output as R-list that captures both inputs and
                output statements
        -   `describe(...)` and variants `? ...` aka describe(…, exact)
            and `?? ...` aka describe(…, inexact)
            -   `describe()` is currently save to use with tell (no
                quotation needed)
            -   `describe()` returns it’s actual output in between
                prompts
        -   `demo("...")`
            -   `demo("...")` needs separate function: `maxima.demo()`
            -   needs it’s own execution function:
                -   after initial execution the input prompt is changed
                    to "\_“. As long as the input prompt has this form,
                    the user sends”;" as input to step through the demo.
                -   implement function `maxima.demo()` that automates
                    this process
                -   the output is the same as for `example()`: return
                    list of the inputs and outputs back to R
    -   catch exceptions
        -   `?? ...` return message that ?? … or describe(…, inexact)
            are not supporte since it enters a user dialog and where
            user needs to choose different option and this is not
            compatible with using it in documents

## Usability

-   remove maxima.start(), maxima should be started if it hasn’t when
    commands are to be executed
-   enable replacing latex math delimiters “$$” by more space-consistent
    variants, e.g. “\[” or `align` environment
-   Provide an interface to plot2d() and plot3d() enableling storing of
    graphics and usage in RMarkdown documents
-   Workflow for working with TeX and MathML output:
    -   enable: dynamically change output display: 1d, tex, mathml
-   implement maxima return type class (S3, S4 or RC?)
    -   maxima returns always a class of this type
    -   the type stores different format types of output strings,
        i.e. linear, latex, MathML,
    -   the type also stores a reference output label
    -   this would be a nice way to handle surpressed outputs from
        maxima
    -   maybe be implement certain operations on those classes
        -   insert parameter values and return calculated function
            values
        -   enable to reinsert objects of this type back into latex

## Portability

-   Remove third party linking entirely
    -   remove dependency from boost::filesystem: use R functions
        instead
-   fix and test for windows, test for MacOS

## Efficiency

-   Remove unneccassary variable conversions from `std::string` to
    `boost::filesystem::path` and vice versa

## Documentation
