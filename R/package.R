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
#' @importFrom utils tail
#' @useDynLib rim, .registration = TRUE
## usethis namespace: start
#'
"_PACKAGE"
#> [1] "_PACKAGE"

maxima.env <- new.env()

#' @describeIn rim-package (re-)starts Maxima.
#' @param restart if FALSE (default), then Maxima is started provided it is not running already. If TRUE starts or restarts Maxima.
#' @export
#' @examples
#' if(maxima.isInstalled()) maxima.start(restart = TRUE)
maxima.start <- function(restart = FALSE) { 
  maxima.env$maxima$start(restart) 
  maxima.options(format = "linear")
  maxima.options(label = TRUE)
}

#' @describeIn rim-package Quits Maxima.
#' @param engine if FALSE (default), quits the (running) maxima instance and closes the connection, otherwise quits and closes the (running) maxima instance used for the knitr engine.
#' @export
#' @examples
#' if(maxima.isInstalled()) {
#'   maxima.start(restart = TRUE)
#'   maxima.stop()
#' }
maxima.stop <- function(engine = FALSE) {
  if(!engine)
    maxima.env$maxima$stop()
  else {
    if(exists("mx", envir = maxima.env))
      maxima.env$mx$stop()
  }
}

#' @describeIn rim-package Executes a single Maxima command provided by \code{command}. If no command ending character (\code{;} or \code{$} is provided, \code{;} is appended.
#' @param command character string containing the Maxima command.
#' @seealso \code{\link{maxima.engine}}, \code{\link{maxima.options}}
#' @export
#' @examples
#' if(maxima.isInstalled()) maxima.get("2+2;")
maxima.get <- function(command) {
  return(maxima.env$maxima$get(command))
}

#' @describeIn rim-package A wrapper to load a Maxima module named by \code{module}
#' @param module character vector naming the Maxima module (typically a *.mac or *.lisp file) to be loaded.
#' @return invisibly returns NULL.
#' @export
#' @examples
#' if(maxima.isInstalled()) maxima.load("ratpow")
maxima.load <- function(module) {
  maxima.env$maxima$loadModule(module) 
}

#' @describeIn rim-package A wrapper to the Maxima helper function \code{apropos} to lookup existing Maxima functions that match \code{keystring}.
#' @param keystring character vector containing a search term.
#' @export
#' @examples
#' if(maxima.isInstalled()) maxima.apropos("integrate")
maxima.apropos <- function(keystring) {
  return(maxima.env$maxima$get(paste0("apropos(\"", keystring, "\");")))
}


#' @describeIn rim-package Returns the version number of Maxima that is used
#' @export
#' @examples
#' maxima.version()
maxima.version <- function() {
  maxima.env$maxima$getVersion()
}

#' @describeIn rim-package Returns TRUE when an installation of Maxima has been detected, otherwise FALSE
#' @export
#' @examples
#' maxima.isInstalled()
maxima.isInstalled <- function() {
  maxima.env$maxima$isInstalled()
}

#' @describeIn rim-package Prints the input command of an maxima S3-object returned by \code{\link{maxima.get}()}
#' @param x S3-Object of class "maxima", the returned type of object from \code{maxima.get()}.
#' @return Character vector of length 1 of the input command. Depending on whether option "label" is set to TRUE, the corresponding input reference label is printed preceding the input command.
#' @export
#' @examples
#' if(maxima.isInstalled()) {
#'   a <- maxima.get("2+2;")
#'   iprint(a)
#' }
iprint <- function(x) {
  stopifnot(isa(x, what = "maxima"))

  # TODO: Determin calling function to distinguish return value
  label <- character(0)
  if(exists("mx", maxima.env)) {
    if(maxima.options$engine.label) 
      label <- paste0("(", attr(x, "input.label"), ") ")
  } else {
    if(maxima.options$label)
      label <- paste0("(", attr(x, "input.label"), ") ")
  }

  paste0(label, attr(x, "command"))
}

#' @describeIn rim-package Prints the maxima output part of an S3 object returned by \code{\link{maxima.get}()} 
#' @param x S3 object of class "maxima"
#' @param ... other arguments (ignored).
#' @method print maxima
#' @export
#' @examples
#' if(maxima.isInstalled()) {
#'   a <- maxima.get("2+2;")
#'   print(a)
#' }
print.maxima <- function(x, ...) {
  if(!attr(x, "suppressed")) {
    if(attr(x, "from_engine") & !is_interactive()) {
      label_opt <- maxima.options$engine.label
      format_opt <- maxima.options$engine.format
    } else {
      label_opt <- maxima.options$label
      format_opt <- maxima.options$format
    }

    label_opt <- ifelse(label_opt, "wtl", "wol")

    txt <- x[[label_opt]][[format_opt]]

    if(is_html_output()) {
      txt <- gsub(pattern = "\\\\%", replacement = "%", x = txt)
    }

    # if(!(attr(x, "from_engine") | is_interactive())) 
    if(is_interactive())
      cat(txt, sep = '\n')
    invisible(txt)
  }
}

#' @describeIn rim-package Evaluates the parsed and quoted R-expression which is part of an S3 object returned by \code{\link{maxima.get}()}
#' @param x S3 object of class "maxima"
#' @param x Either a character vector of length 1L or an S3 object of class "maxima"
#' @param code A logical vector of length 1L, whether to attach the original expression (TRUE) or not (FALSE, default)
#' @param envir A environment object. \code{globalenv()} (default), is passed to eval().
#' @return The evaluated R-object
#' @export
#' @examples
#' if(maxima.isInstalled()) {
#'   a <- maxima.get("2+2;")
#'   maxima.eval(a)
#'   # same
#'   maxima.eval("2+2;")
#'   # evaluate with data.frame
#'   df <- data.frame(x = seq(0, 1, by = 0.1))
#'   maxima.eval("integrate(1 / (1 + x^4), x);", code = TRUE, envir = df)
#'   maxima.stop()
#' }
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
