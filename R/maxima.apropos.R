#' Uses maximas lookup function \code{apropos()} and retrieves result
#' 
#' @param command A character vector containing the maxima command.
#' @return A character vector containing the corresponding output from maxima
#' @export
#' @examples
#' maxima.start()
#'
#' maxima.apropos("int")
maxima.apropos <- function(keystring) maxima$apropos(keystring) 

