#' Stoping maxima process
#' 
#' Calling this function stops the maxima child process and deletes the corresponding objects 
#' 
#' @return No return value
#' @family rmaxima functions
#' @export
maxima.stop <- function() {
	maxima$stopMaxima()
}

