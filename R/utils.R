switch_format <- function(format = "linear") {
  stopifnot(is.character(format))
  switch(format,
	 "latex" = format,
	 "linear" = format,
	 "ascii" = format,
	 "mathml" = format,
	 format <- "linear"
  )
  return(format)
}

#' Check input and guarantee to return one of the elements of \code{set}. A generalisation of function \code{switch_format}
#' @param x Character vector of length 1. 
#' @param set Character vector. Contains the options to check \code{x} against.
#' @return Character vector of length 1, one element of \code{set}. If \code{x} does not match exactly any of the elements in \code{set}, then the first element of \code{set} is returned.
#' @noRd 
rectify <- function(x, set = c("linear", "ascii", "latex", "mathml"), warn = TRUE) {
  stopifnot(length(x) == 1 && is.character(x))
  if(any(x %in% set))
    return(x)
  else {
    if(warn)
      warning(paste0("'", x, "' does not match given options: ", paste0("'", set, "'", collapse = ", "), "\nUsing '", set[1], "' instead."))
    invisible(set[1])
  }
}

function.frame <- function(name) {
  all <- sys.calls()
  all <- lapply(FUN=deparse, X=all)
  which(grepl(pattern=name, x = all))
}
