
## Up until R 2.15.0, the require("methods") is needed but (now)
## triggers an warning from R CMD check
#.onLoad <- function(libname, pkgname){
#    #require("methods")  ## needed with R <= 2.15.0
#    loadRcppModules()
#}


## For R 2.15.1 and later this also works. Note that calling loadModule() triggers
## a load action, so this does not have to be placed in .onLoad() or evalqOnLoad().

#' @importFrom Rcpp loadModule
loadModule("Maxima", TRUE)

# .onAttach <- function(libname, pkgname) {
# 	"package:base" %in% search()
# 	maxima.start <<- function() maxima <<- new(RMaxima)
# 	maxima.tell <<- function(command) maxima$execute(command) 
# }

.onUnload <- function (libpath) {
  library.dynam.unload("rmaxima", libpath)
}
