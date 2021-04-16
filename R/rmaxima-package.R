#' rmaxima
#'
#' @description
#' This the description bla bla bla
#'
#' @details
#' Here comes the details section
#'
#' @keywords internal
#' @import methods
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib rmaxima, .registration = TRUE
## usethis namespace: start
#'
"_PACKAGE"
#> [1] "_PACKAGE"

maxima.env <- new.env()
maxima.env$format <- "linear"

#' @rdname rmaxima-package
#' @param restart If FALSE (default), then maxima process is started provided it is not running already. If TRUE starts or restarts maxima.
#' @export
maxima.start <- function(restart = FALSE) { 
	maxima.env$maxima$startMaxima(restart) 
}

#' @describeIn rmaxima-package Quits the running maxima process.
#' @export
maxima.stop <- function() {
	maxima.env$maxima$stopMaxima()
}

#' @describeIn rmaxima-package Executes a single Maxima command provided by \code{command}.
#' @param command A character vector containing the maxima command.
#' @export
maxima.get <- function(command) maxima.env$maxima$execute(command) 

#' @describeIn rmaxima-package A wrapper function to load a Maxima module named by \code{module}
#' @param module A character vector naming the maxima module to be loaded.
#' @export
maxima.load <- function(module) maxima.env$maxima$loadModule(module) 

#' @describeIn rmaxima-package A wrapper function to call the Maxima helper function \code{apropos} on a search term provided by \code{keystring}
#' @param command A character vector containing the maxima command.
#' @export
maxima.apropos <- function(keystring) maxima.env$maxima$apropos(keystring) 


#' @describeIn rmaxima-package Sets the format of the output string from Maxima.
#' @param format A character vector naming the output display format. Can be one of \code{linear} (default), \code{latex} (i.e. $$...$$), \code{text}.
#' @export
maxima.setformat <- function(format = "linear") {
	if(!is.character(format))
		stop("Invalid input: expected character vector")

	switch(format,
	       latex = maxima.env$maxima$loadInit("maxima-init-tex2.mac"),
	       linear = maxima.env$maxima$loadInit("maxima-init-lin.mac"),
	       text = maxima.env$maxima$loadInit("maxima-init-lin.mac"),
	       default = maxima.env$maxima$loadInit("maxima-init-lin.mac")
	       )

	maxima.env$format <- format
}

#' @describeIn rmaxima-package Returns the currently set format as a character vector
#' @export
maxima.getformat <- function() {
	maxima.env$format
}
