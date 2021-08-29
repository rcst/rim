# utils::globalVariables(c("engine", "engine.format", "engine.reflabels", "mx"))
#' knitr maxima engine
#'
#' Functions to process Maxima code chunks by \code{knitr}.
#'
#' Upon attachement, i.e. \code{library(rim)} function \code{maxima.engine} is registered as a \code{knitr} engine. Thus, \code{maxima.engine()} is called by \code{knit()} to evaluate Maxima code chunks. When called upon the first code chunk of a document it starts Maxima in a separate process in server mode. This means that a single Maxima session is used for all Maxima code chunks of an RMarkdown document. Inputs and outputs can thus be used across chunks (e.g. by using Maxima reference labels). \code{maxima.options(engine.format = ..., engine.label = ...)} configures the output format and whether or not output reference labels should be printed.
#'
#' The purpose of \code{maxima.inline} is to insert Maxima results as inline text, i.e. on the same line of the preceding text, if it is actually written on the same line of the RMarkdown file. It uses the same running Maxima process as \code{maxima.engine}. The output format for inline results can be configured separately from the settings of \code{maxima.engine}, i.e. \code{maxima.options(inline.format = ..., inline.label = ...)}.
#'
#' @param options Named \code{list} of knitr options. Currently there are no maxima specific chunk options. To change the output format of the maxima engine set the variable \code{maxima.engine.format} to either "linear" (default), "latex", "mathml" or "text2d".
#' 
#' @import knitr
#' @importFrom utils tail
#'
#' @return This functions prints the resulting output from maxima together with it's code 
maxima.engine <- function(options) { 
  maxima.engine.start()
  code <- options$code
  code <- code[nchar(code) > 0]
  cmds <- gather(code)
  ll <- list()
  ccode <- character()
  for(i in 1:length(cmds)) {
    tt <- maxima.env$mx$get(paste0(code[cmds[[i]]], collapse = "\n"))
    # if(maxima.env$engine.format == "text2d") { 
    #   if(!maxima.env$engine.reflabels)
    #      tt <- str_strip_col(x = tt, n = nchar(tt$outputLabel), side = "left") 
    # }
    # else 
    ccode <- append(ccode, iprint(tt))

    if(!attr(tt, "suppressed")) {
      ll <- append(ll, list(structure(list(src = ccode), class = "source")))
      ll <- append(ll, engine_print(tt))
      ccode <- character()
    }
  }

  if(length(ccode))
    ll <- append(ll, list(structure(list(src = ccode), class = "source")))


  if (last_label(options$label)) { 
    maxima.engine.stop()
  }

  # engine_output(options, options$code, out)
  engine_output(options, out = ll)
}

maxima.engine.start <- function() {
  if(!exists("mx", envir = maxima.env)) { 
    maxima.env$mx <- RMaxima$new(display = maxima.options$display)
    maxima.env$engine.reflabels <- TRUE
  }
}

maxima.engine.stop <- function() { 
  maxima.env$mx$stop()
  rm(mx, 
     envir = maxima.env) 
}

last_label <- function(label = knitr::opts_current$get('label')) {
  # if (knitr:::child_mode()) return(FALSE)
  if(knitr::opts_knit$get('child')) return(FALSE)
  labels <- knitr::all_labels(engine == 'maxima')
  tail(labels, 1) == label
}

engine_print <- function(x){
  switch(maxima.options$engine.label + 1,
	 paste0(c(x[["wol"]][[maxima.options$engine.format]], ""), collapse = "\n"),
	 paste0(c(x[["wtl"]][[maxima.options$engine.format]], ""), collapse = "\n"))
}

#' @describeIn maxima.engine This function can be used to insert maxima outputs as inline.
#' @param command Character vector of length 1 containing the Maxima command to be executed
#' @return Character string of length 1 containing the maxima result printed according options set by \code{maxima.options(inline.format = ..., inline.label = ...)}
#' @export
maxima.inline <- function(command) {
  maxima.engine.start()
  x <- maxima.env$mx$get(command)

  switch(maxima.options$inline.label + 1,
	 paste0(c(x[["wol"]][[maxima.options$inline.format]], ""), collapse = "\n"),
	 paste0(c(x[["wtl"]][[maxima.options$inline.format]], ""), collapse = "\n"))
}
