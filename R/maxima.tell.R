#' Execute maxima command and retrieve result
#' 
#' @param command A character vector containing the maxima command.
#' @return A character vector containing the corresponding output from maxima
#' @export
#' @examples
#' m <- new(RMaxima)
#'
#' maxima.tell("2+2")
#' maxima.tell("2+2;")
#' maxima.tell("jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] )")
maxima.tell <- function(command) maxima$execute(command) 
