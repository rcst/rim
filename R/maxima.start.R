#' Starting maxima
#' 
#' Calling this function starts maxima in a child process in the background and configures the display output of maxima
#' 
#' @return No return value
#' @family rmaxima functions
#' @export
maxima.start <- function(restart = FALSE) { 
	maxima$startMaxima(restart) 
}

