#' Let maxima load a module
#' 
#' @param module A character vector naming the maxima module to be loaded.
#' @return A character vector containing the corresponding output from maxima
#' @export
#' @examples
#' maxima.load("ctensor")
maxima.load <- function(module) maxima$loadModule(command) 

