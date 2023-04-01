#' rim
#'
#' @description
#' Provides an interface to Maxima, a computer algebra system.
#'
#' @details
#' Note: You need to install the Maxima software separately in order to make use of this package. 
#' 
#' Maxima is set up automatically on attachment via \code{library(rim)} and automatically started when a command is send (if it isn't running already) using \code{\link{maxima.get}()}. If environment variable RIM_MAXIMA_PATH is not set, rim will search for the Maxima executable, or use the former otherwise. Using \code{\link{maxima.start}()} and \code{\link{maxima.stop}()}, one can stop and (re-)start the current Maxima session if needed, e.g. to clear Maxima command and output history.
#'
#' To send a single command to Maxima and receive the corresponding output use \code{\link{maxima.get}()}. This function returns a S3 object of class "maxima". The output is printed by printing the object and will be printed in a format currently set by \code{\link{maxima.options}(format)}. The output format can be changed by setting it, e.g. \code{\link{maxima.options}(format = "ascii")}. Output labels are printed according to option \code{\link{maxima.options}(label)}.
#'
#' @import methods
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib rim, .registration = TRUE
## usethis namespace: start
#'
"_PACKAGE"
#> [1] "_PACKAGE"

maxima.env <- new.env()

#' @describeIn rim-package (re-)starts Maxima.
#' @param restart if FALSE (default), then Maxima is started provided it is not running already. If TRUE starts or restarts Maxima.
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
#' @param command character string containing the Maxima command.
#' @seealso \code{\link{maxima.engine}}, \code{\link{maxima.options}}
#' @export
maxima.get <- function(command) {
  return(maxima.env$maxima$get(command))
}

#' @describeIn rim-package A wrapper to load a Maxima module named by \code{module}
#' @param module character vector naming the Maxima module (typically a *.mac or *.lisp file) to be loaded.
#' @return invisibly returns NULL.
#' @export
maxima.load <- function(module) {
  maxima.env$maxima$loadModule(module) 
}

#' @describeIn rim-package A wrapper to the Maxima helper function \code{apropos} to lookup existing Maxima functions that match \code{keystring}.
#' @param keystring character vector containing a search term.
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

#' @describeIn rim-package Prints the input command of an maxima S3-object returned by \code{\link{maxima.get}()}
#' @param x S3-Object of class "maxima", the returned type of object from \code{maxima.get()}.
#' @return Character vector of length 1 of the input command. Depending on whether option "label" is set to TRUE, the corresponding input reference label is printed preceding the input command.
#' @export
iprint <- function(x) {
  stopifnot(isa(x, what = "maxima"))
  # cll <- deparse(sys.call())
  # cll <- deparse(sys.calls()[[sys.parent()]])
  if(exists("mx", maxima.env)) {
  # if(grepl("^engine\\(options\\)$", cll)) {
    if(maxima.options$engine.label) 
      paste0("(", attr(x, "input.label"), ") ", attr(x, "command"))
    else
      paste0(attr(x, "command"))
  }
  else {
    if(maxima.options$label) 
      paste0("(", attr(x, "input.label"), ") ", attr(x, "command"))
    else
      paste0(attr(x, "command"))
  }
}

#' @describeIn rim-package Prints the maxima output part of an S3 object returned by \code{\link{maxima.get}()} 
#' @param x S3 object of class "maxima"
#' @param ... other arguments (ignored).
#' @method print maxima
#' @export
print.maxima <- function(x, ...) {
  if(!attr(x, "suppressed")) {
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
}

#' @describeIn rim-package Evaluates the parsed and quoted R-expression which is part of an S3 object returned by \code{\link{maxima.get}()}
#' @param x S3 object of class "maxima"
#' @param x Either a character vector of length 1L or an S3 object of class "maxima"
#' @param code A logical vector of length 1L, whether to attach the original expression (TRUE) or not (FALSE, default)
#' @param envir A environment object. \code{globalenv()} (default), is passed to eval().
#' @return The evaluated R-object
#' @export
maxima.eval <- function(x, code = FALSE, envir = globalenv()) {
	expr <- NA
	if(is.character(x))
		x <- maxima.get(x)
	if(is(x, "maxima"))
		expr <- attr(x, "parsed")
	r <- eval(expr, envir = envir)
	if(code) 
		attr(r, "maxima") <- expr
	return(r)
}
