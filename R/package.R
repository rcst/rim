#' rim
#'
#' @description
#' Provides an interface to Maxima, a computer algebra system.
#'
#' @details
#' Note: You need to install the Maxima software separately in order to make use of this package. 
#' 
#' Maxima is set up automatically on attachment via \code{library(rim)} and automatically started when a command is send (if it isn't running already) using \code{maxima.get()}. Using \code{maxima.start()} and \code{maxima.stop()}, one can stop and (re-)start the current Maxima session if needed, e.g. to clear Maxima command and output history.
#'
#' To send a single command to Maxima and receive the corresponding output use \code{maxima.get()}. The output is returned in the format currently set (\code{maxima.getformat()}). The format can be changed using \code{maxima.setformat())}.
#'
#' @import methods
#' @import digest 
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib rim, .registration = TRUE
## usethis namespace: start
#'
"_PACKAGE"
#> [1] "_PACKAGE"

maxima.env <- new.env()

#' @describeIn rim-package (re-)starts Maxima.
#' @param restart If FALSE (default), then Maxima is started provided it is not running already. If TRUE starts or restarts Maxima.
#' @export
maxima.start <- function(restart = FALSE) { 
  maxima.env$maxima$start(restart) 
  maxima.options(format = "linear")
  maxima.options(label = TRUE)
}

#' @describeIn rim-package Quits Maxima.
#' @export
maxima.stop <- function() {
  maxima.env$maxima$stop()
}

#' @describeIn rim-package Executes a single Maxima command provided by \code{command}. If no command ending character (\code{;} or \code{$} is provided, \code{;} is appended.
#' @param command A character vector containing the maxima command.
#' @seealso \code{\link{maxima.engine}}
#' @export
maxima.get <- function(command) {
  return(maxima.env$maxima$get(command))
}

#' @describeIn rim-package A wrapper to load a Maxima module named by \code{module}
#' @param module A character vector naming the maxima module to be loaded.
#' @return Invisibly returns NULL.
#' @export
maxima.load <- function(module) {
  maxima.env$maxima$loadModule(module) 
  invisible(NULL)
}

#' @describeIn rim-package A wrapper to the Maxima helper function \code{apropos} to lookup existing Maxima functions that match \code{keystring}.
#' @param keystring A character vector containing a search term.
#' @export
maxima.apropos <- function(keystring) {
  return(maxima.env$maxima$get(paste0("apropos(\"", keystring, "\");")))
}


#' @describeIn rim-package Returns the version number of Maxima that is used
#' @export
maxima.version <- function() {
  maxima.env$maxima$getVersion()
}

#' @describeIn rim-package Returns TRUE when an installation of Maxima has been detected, otherwise FALSE
#' @export
maxima.isInstalled <- function() {
  maxima.env$maxima$isInstalled()
}

#' @describeIn rim-package Prints the input command preceding with the corresponding input reference label of an maxima S3-object returned by maxima.get()
#' @export
iprint <- function(x) {
  if(class(x) != "maxima")
    stop("x is not a maxima object")

  paste0("(", attr(x, "input.label"), ") ", attr(x, "command"))
}

#' @describeIn rim-package Prints the maxima output part of an S3 object returned by maxima.get() 
#' @param x S3 object of class "maxima"
#' @param ... Additional arguments
#' @method print maxima
#' @export
print.maxima <- function(x) {
  switch(maxima.options$label + 1,
	 {
	   cat(paste0(c(x[["wol"]][[maxima.options$format]], ""), collapse = "\n"))
	   invisible(paste0(c(x[["wol"]][[maxima.options$format]], ""), collapse = "\n"))
	 },
	 {
	   cat(paste0(c(x[["wtl"]][[maxima.options$format]], ""), collapse = "\n"))
	   invisible(paste0(c(x[["wol"]][[maxima.options$format]], ""), collapse = "\n"))
	 }
  )
}
