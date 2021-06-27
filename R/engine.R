utils::globalVariables(c("engine", "engine.format", "engine.reflabels", "mx"))
#' knitr maxima engine
#'
#' An R-function that is registered as a knitr engine when package \code{rim} is attached, i.e. \code{library(rim)}. 
#'
#' \code{maxima.engine} is called by \code{knit()} to evaluate maxima code chunks. When called upon the first code chunk of a document it runs Maxima in the in a separate process in server mode. This means that a single Maxima session is used for all Maxima code chunks of an RMarkdown document. Inputs and outputs can thus be used across chunks (using e.g. Maxima reference labels).  
#'
#' In addition, this function sets up Maxima specific output and chunk hooks to be used via chunk options.
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
  # code <- collect_ends(code)
  code <- code[nchar(code) > 0]
  cmds <- gather(code)
  ll <- list()
  ccode <- character()
  for(i in 1:length(cmds)) {
    tt <- maxima.env$mx$get(paste0(code[cmds[[i]]], collapse = "\n"))
    if(maxima.env$engine.format == "text2d") { 
      if(!maxima.env$engine.reflabels)
	 tt <- str_strip_col(x = tt, n = nchar(tt$outputLabel), side = "left") 
    }
    else 
      ccode <- append(ccode, iprint(tt))
      if(!attr(tt, "suppressed")) {
	ll <- append(ll, list(structure(list(src = ccode), class = "source")))
	ll <- append(ll, tt)
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
    maxima.env$mx <- RMaxima$new(display = maxima.env$display)
    maxima.env$engine_ref_labels <- TRUE
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
