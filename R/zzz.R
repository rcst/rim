
## Up until R 2.15.0, the require("methods") is needed but (now)
## triggers an warning from R CMD check
#.onLoad <- function(libname, pkgname){
#    #require("methods")  ## needed with R <= 2.15.0
#    loadRcppModules()
#}


## For R 2.15.1 and later this also works. Note that calling loadModule() triggers
## a load action, so this does not have to be placed in .onLoad() or evalqOnLoad().

#' @importFrom Rcpp loadModule
# loadModule("Maxima", TRUE)

.onAttach <- function(libname, pkgname) {
  maxima.env$maxima <- RMaxima$new()
  if(requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(maxima = maxima.engine)
    setup_hooks()
    packageStartupMessage("Maxima successfully registered as knitr engine!")
  } else
    packageStartupMessage("Install package knitr if you want to register maxima a knitr engine first")
}

.onUnload <- function (libpath) { 
  library.dynam.unload("rim", libpath)
  rm("maxima", envir = maxima.env)
}

.onDetach <- function(libpath) {
  maxima.env$maxima.stop()
}
