rim - R’s interface to Maxima
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![](https://cranlogs.r-pkg.org/badges/rim)](https://CRAN.R-project.org/package=rim)
[![](https://cranlogs.r-pkg.org/badges/grand-total/rim)](https://CRAN.R-project.org/package=rim)

Development version of CRAN package. `rim` provides an interface to the
powerful and fairly complete maxima computer algebra system

This repository is a fork from RMaxima, the original version created by
Kseniia Shumelchyk and Hans W. Borcher
[shumelchyk/rmaxima](https://github.com/shumelchyk/rmaxima), which is
currently not maintained.

# Installation

``` r
install.packages("rim")
```

## Requirements

- Maxima (<https://sourceforge.net/projects/maxima/>, tested with
  versions \>=5.42.1), needs to be on PATH

## Latest Version

If you want to install the latest version install the R package
`remotes` first and then install the package from this github repo:

``` r
install.packages("remotes")
remotes::install_github("rcst/rim")
```

# Usage

This section only demonstrates the package’s R-function directly
accessible to the user. On how to use the package’s `knitr` engine see
[this page](https://rcst.github.io/rim/).

``` r
library(rim)
```

``` r
maxima.start(restart = TRUE)
maxima.get("1+1;")
r <- maxima.get("sum(1/x^2, x, 1, 10000)")
```

    ## Warning in value[[3L]](cond): Caught error while parsing
    ## input buffer overflow (<input>:1:8193)
    ## Returning NA.

``` r
print(r)
iprint(r)
```

    ## [1] "(%i2) sum(1/x^2, x, 1, 10000)"

``` r
maxima.isInstalled()
```

    ## [1] TRUE

``` r
maxima.version()
```

    ## [1] '5.47.0'

``` r
maxima.options
```

    ##  Option        Value 
    ##  -------------:-----------------------------------------------------------------
    ##  format        linear
    ##                (Printing format of returned object from maxima.get())
    ##  engine.format latex 
    ##                (Same as 'format', but for maxima code chunks in
    ##                'RMarkdown' documents.)
    ##  inline.format inline
    ##                (Same as 'engine.format', but for printing output inline
    ##                via maxima.inline(). Cannot be set to 'ascii'.)
    ##  label         TRUE  
    ##                (Sets whether a maxima reference label should be printed
    ##                when printing a maxima return object.)
    ##  engine.label  TRUE  
    ##                (Same as 'label', but for maxima code chunks.)
    ##  inline.label  FALSE 
    ##                (Same as 'label', but for inline code chunks)

``` r
maxima.options(format = "ascii")
maxima.apropos("int")
maxima.get("integrate(1 / (x^4 + 1), x);")
maxima.get("jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] )")
l <- maxima.get("%;")
maxima.eval(l, code = TRUE, envir = list(alpha = 0.3, beta = 2.5))
```

    ##             [,1]       [,2]
    ## [1,]  0.31887755 -0.1067168
    ## [2,] -0.03826531 -0.1067168
    ## attr(,"maxima")
    ## matrix(data = c(((-1L * alpha * ((alpha + beta)^-2L)) + ((alpha + 
    ##     beta)^-1L)), (-1L * alpha * ((alpha + beta)^-2L)), ((-1L/2L) * 
    ##     ((alpha + beta)^(-3L/2L))), ((-1L/2L) * ((alpha + beta)^(-3L/2L)))), 
    ##     ncol = 2, nrow = 2)

``` r
unclass(l)
```

    ## $wtl
    ## $wtl$linear
    ## [1] "(%o6) matrix([1/(beta+alpha)-alpha/(beta+alpha)^2,-(alpha/(beta+alpha)^2)],"
    ## [2] "             [-(1/(2*(beta+alpha)^(3/2))),-(1/(2*(beta+alpha)^(3/2)))])"    
    ## 
    ## $wtl$ascii
    ## [1] "           [      1              alpha                alpha        ]"
    ## [2] "           [ ------------ - ---------------    - ---------------   ]"
    ## [3] "           [ beta + alpha                 2                    2   ]"
    ## [4] "           [                (beta + alpha)       (beta + alpha)    ]"
    ## [5] "(%o6)      [                                                       ]"
    ## [6] "           [                1                           1          ]"
    ## [7] "           [     - -------------------       - ------------------- ]"
    ## [8] "           [                       3/2                         3/2 ]"
    ## [9] "           [       2 (beta + alpha)            2 (beta + alpha)    ]"
    ## 
    ## $wtl$latex
    ## [1] "$$\\mathtt{(\\textit{\\%o}_{6})}\\quad \\begin{pmatrix}\\frac{1}{\\beta+\\alpha}-\\frac{\\alpha}{\\left(\\beta+\\alpha\\right)^2} & -\\left(\\frac{\\alpha}{\\left(\\beta+\\alpha\\right)^2}\\right) \\\\ -\\left(\\frac{1}{2\\,\\left(\\beta+\\alpha\\right)^{\\frac{3}{2}}}\\right) & -\\left(\\frac{1}{2\\,\\left(\\beta+\\alpha\\right)^{\\frac{3}{2}}}\\right) \\\\ \\end{pmatrix}$$"
    ## 
    ## $wtl$inline
    ## [1] "$\\mathtt{(\\textit{\\%o}_{6})}\\quad \\begin{pmatrix}\\frac{1}{\\beta+\\alpha}-\\frac{\\alpha}{\\left(\\beta+\\alpha\\right)^2} & -\\left(\\frac{\\alpha}{\\left(\\beta+\\alpha\\right)^2}\\right) \\\\ -\\left(\\frac{1}{2\\,\\left(\\beta+\\alpha\\right)^{\\frac{3}{2}}}\\right) & -\\left(\\frac{1}{2\\,\\left(\\beta+\\alpha\\right)^{\\frac{3}{2}}}\\right) \\\\ \\end{pmatrix}$"
    ## 
    ## $wtl$mathml
    ##  [1] " <math xmlns=\"http://www.w3.org/1998/Math/MathML\"> <mi>mlabel</mi> " 
    ##  [2] " <mfenced separators=\"\"><msub><mi>%o</mi> <mn>6</mn></msub> "        
    ##  [3] " <mo>,</mo><mfenced separators=\"\" open=\"(\" close=\")\"><mtable>"   
    ##  [4] " <mtr><mtd><mfrac><mrow><mn>1</mn> </mrow> <mrow><mi>&beta;</mi> "     
    ##  [5] " <mo>+</mo> <mi>&alpha;</mi> </mrow></mfrac> <mo>-</mo> <mfrac><mrow>" 
    ##  [6] " <mi>&alpha;</mi> </mrow> <mrow><msup><mrow><mfenced separators=\"\">" 
    ##  [7] " <mi>&beta;</mi> <mo>+</mo> <mi>&alpha;</mi> </mfenced> </mrow> "      
    ##  [8] " <mn>2</mn> </msup> </mrow></mfrac> </mtd><mtd><mo>-</mo>"             
    ##  [9] " <mfrac><mrow><mi>&alpha;</mi> </mrow> <mrow><msup><mrow>"             
    ## [10] " <mfenced separators=\"\"><mi>&beta;</mi> <mo>+</mo> <mi>&alpha;</mi> "
    ## [11] " </mfenced> </mrow> <mn>2</mn> </msup> </mrow></mfrac> </mtd></mtr> "  
    ## [12] " <mtr><mtd><mo>-</mo><mfrac><mrow><mn>1</mn> </mrow> <mrow>"           
    ## [13] " <mn>2</mn> <mspace width=\"thinmathspace\"/><msup><mrow>"             
    ## [14] " <mfenced separators=\"\"><mi>&beta;</mi> <mo>+</mo> <mi>&alpha;</mi> "
    ## [15] " </mfenced> </mrow> <mrow><mfrac><mrow><mn>3</mn> </mrow> <mrow>"      
    ## [16] " <mn>2</mn> </mrow></mfrac> </mrow></msup> </mrow></mfrac> "           
    ## [17] " </mtd><mtd><mo>-</mo><mfrac><mrow><mn>1</mn> </mrow> <mrow>"          
    ## [18] " <mn>2</mn> <mspace width=\"thinmathspace\"/><msup><mrow>"             
    ## [19] " <mfenced separators=\"\"><mi>&beta;</mi> <mo>+</mo> <mi>&alpha;</mi> "
    ## [20] " </mfenced> </mrow> <mrow><mfrac><mrow><mn>3</mn> </mrow> <mrow>"      
    ## [21] " <mn>2</mn> </mrow></mfrac> </mrow></msup> </mrow></mfrac> "           
    ## [22] " </mtd></mtr> </mtable></mfenced> </mfenced> </math>"                  
    ## 
    ## 
    ## $wol
    ## $wol$linear
    ## [1] "matrix([1/(beta+alpha)-alpha/(beta+alpha)^2,-(alpha/(beta+alpha)^2)],"
    ## [2] "       [-(1/(2*(beta+alpha)^(3/2))),-(1/(2*(beta+alpha)^(3/2)))])"    
    ## 
    ## $wol$ascii
    ## [1] "[      1              alpha                alpha        ]"
    ## [2] "[ ------------ - ---------------    - ---------------   ]"
    ## [3] "[ beta + alpha                 2                    2   ]"
    ## [4] "[                (beta + alpha)       (beta + alpha)    ]"
    ## [5] "[                                                       ]"
    ## [6] "[                1                           1          ]"
    ## [7] "[     - -------------------       - ------------------- ]"
    ## [8] "[                       3/2                         3/2 ]"
    ## [9] "[       2 (beta + alpha)            2 (beta + alpha)    ]"
    ## 
    ## $wol$latex
    ## [1] "$$\\begin{pmatrix}\\frac{1}{\\beta+\\alpha}-\\frac{\\alpha}{\\left(\\beta+\\alpha\\right)^2} & -\\left(\\frac{\\alpha}{\\left(\\beta+\\alpha\\right)^2}\\right) \\\\ -\\left(\\frac{1}{2\\,\\left(\\beta+\\alpha\\right)^{\\frac{3}{2}}}\\right) & -\\left(\\frac{1}{2\\,\\left(\\beta+\\alpha\\right)^{\\frac{3}{2}}}\\right) \\\\ \\end{pmatrix}$$"
    ## 
    ## $wol$inline
    ## [1] "$\\begin{pmatrix}\\frac{1}{\\beta+\\alpha}-\\frac{\\alpha}{\\left(\\beta+\\alpha\\right)^2} & -\\left(\\frac{\\alpha}{\\left(\\beta+\\alpha\\right)^2}\\right) \\\\ -\\left(\\frac{1}{2\\,\\left(\\beta+\\alpha\\right)^{\\frac{3}{2}}}\\right) & -\\left(\\frac{1}{2\\,\\left(\\beta+\\alpha\\right)^{\\frac{3}{2}}}\\right) \\\\ \\end{pmatrix}$"
    ## 
    ## $wol$mathml
    ##  [1] " <math xmlns=\"http://www.w3.org/1998/Math/MathML\"> "                 
    ##  [2] " <mfenced separators=\"\" open=\"(\" close=\")\"><mtable><mtr><mtd>"   
    ##  [3] " <mfrac><mrow><mn>1</mn> </mrow> <mrow><mi>&beta;</mi> <mo>+</mo> "    
    ##  [4] " <mi>&alpha;</mi> </mrow></mfrac> <mo>-</mo> <mfrac><mrow>"            
    ##  [5] " <mi>&alpha;</mi> </mrow> <mrow><msup><mrow><mfenced separators=\"\">" 
    ##  [6] " <mi>&beta;</mi> <mo>+</mo> <mi>&alpha;</mi> </mfenced> </mrow> "      
    ##  [7] " <mn>2</mn> </msup> </mrow></mfrac> </mtd><mtd><mo>-</mo>"             
    ##  [8] " <mfrac><mrow><mi>&alpha;</mi> </mrow> <mrow><msup><mrow>"             
    ##  [9] " <mfenced separators=\"\"><mi>&beta;</mi> <mo>+</mo> <mi>&alpha;</mi> "
    ## [10] " </mfenced> </mrow> <mn>2</mn> </msup> </mrow></mfrac> </mtd></mtr> "  
    ## [11] " <mtr><mtd><mo>-</mo><mfrac><mrow><mn>1</mn> </mrow> <mrow>"           
    ## [12] " <mn>2</mn> <mspace width=\"thinmathspace\"/><msup><mrow>"             
    ## [13] " <mfenced separators=\"\"><mi>&beta;</mi> <mo>+</mo> <mi>&alpha;</mi> "
    ## [14] " </mfenced> </mrow> <mrow><mfrac><mrow><mn>3</mn> </mrow> <mrow>"      
    ## [15] " <mn>2</mn> </mrow></mfrac> </mrow></msup> </mrow></mfrac> "           
    ## [16] " </mtd><mtd><mo>-</mo><mfrac><mrow><mn>1</mn> </mrow> <mrow>"          
    ## [17] " <mn>2</mn> <mspace width=\"thinmathspace\"/><msup><mrow>"             
    ## [18] " <mfenced separators=\"\"><mi>&beta;</mi> <mo>+</mo> <mi>&alpha;</mi> "
    ## [19] " </mfenced> </mrow> <mrow><mfrac><mrow><mn>3</mn> </mrow> <mrow>"      
    ## [20] " <mn>2</mn> </mrow></mfrac> </mrow></msup> </mrow></mfrac> "           
    ## [21] " </mtd></mtr> </mtable></mfenced> </math>"                             
    ## 
    ## $wol$rstr
    ## [1] "matrix(data = c(((-1L * alpha * ((alpha + beta) ^ -2L)) + ((alpha + beta) ^ -1L)), (-1L * alpha * ((alpha + beta) ^ -2L)), ((-1L / 2L) * ((alpha + beta) ^ (-3L / 2L))), ((-1L / 2L) * ((alpha + beta) ^ (-3L / 2L)))), ncol = 2, nrow = 2)"
    ## 
    ## 
    ## attr(,"input.label")
    ## [1] "%i6"
    ## attr(,"output.label")
    ## [1] "%o6"
    ## attr(,"command")
    ## [1] "%;"
    ## attr(,"suppressed")
    ## [1] FALSE
    ## attr(,"parsed")
    ## matrix(data = c(((-1L * alpha * ((alpha + beta)^-2L)) + ((alpha + 
    ##     beta)^-1L)), (-1L * alpha * ((alpha + beta)^-2L)), ((-1L/2L) * 
    ##     ((alpha + beta)^(-3L/2L))), ((-1L/2L) * ((alpha + beta)^(-3L/2L)))), 
    ##     ncol = 2, nrow = 2)
    ## attr(,"from_engine")
    ## [1] FALSE

``` r
maxima.load("abs_integrate")
maxima.stop()
```
