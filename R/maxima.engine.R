utils::globalVariables(c("engine", "engine_format", "engine_ref_labels", "mx"))
#' knitr maxima engine
#'
#' An R-function that is registered as a knitr engine when package \code{rmaxima} is attached, i.e. \code{library(rmaxima)}. 
#'
#' \code{maxima.engine} is called by \code{knit()} to evaluate maxima code chunks. When called upon the first code chunk of a document it runs Maxima in the in a separate process in server mode. This means that a single Maxima session is used for all Maxima code chunks of an RMarkdown document. Inputs and outputs can thus be used across chunks (using e.g. Maxima reference labels).  
#'
#' In addition, this function sets up Maxima specific output and chunk hooks to be used via chunk options.
#' 
#' @import knitr
#' @importFrom utils tail
#' @param maxima.format A character vector of length 1. Can be one of "linear" (default), "latex", "text2d" or "mathml". Changes the output format of Maxima for the respective code chunk.
#'
#' @param maxima.label Logical. If TRUE output reference labels are printed additionally, otherwise not (default).
#'
#' @return This functions prints the resulting output from maxima together with it's code 
maxima.engine <- function(options) { 
  maxima.engine.start()

  code <- options$code
  code <- collect_ends(code)
  # out <- character(0)
  ll <- list()
  for(i in 1:length(code)) {
    if(maxima.env$engine_format == "text2d") { 
      tt <- maxima.env$mx$get(code[i], FALSE)
      if(!maxima.env$engine_ref_labels)
	 tt <- str_strip_col(x = tt, n = label_length(tt), side = "left") 
    }
    else 
      tt <- maxima.env$mx$get(code[i], maxima.env$engine_ref_labels)

    ll <- append(ll, list(structure(list(src = iprint(tt)), class = "source")))
    
    # out <- c(out, tt) 
    ll <- append(ll, tt)
  }

  if (last_label(options$label)) { 
    maxima.engine.stop()
  }

  # engine_output(options, options$code, out)
  engine_output(options, out = ll)
}

maxima.engine.start <- function() {
  if(!exists("mx", envir = maxima.env)) { 
    # maxima.env$mx <- new(RMaxima)
    maxima.env$mx <- RMaxima$new()
    maxima.env$engine_format <- "linear"
    maxima.env$engine_ref_labels <- TRUE
  }
}

maxima.engine.stop <- function() { 
  # maxima.env$mx$stopMaxima() 
  maxima.env$mx$stop()
  rm(mx, 
     engine_format,
     engine_ref_labels,
     envir = maxima.env) 
}


last_label <- function(label = knitr::opts_current$get('label')) {
  # if (knitr:::child_mode()) return(FALSE)
  if(knitr::opts_knit$get('child')) return(FALSE)
  labels <- knitr::all_labels(engine == 'maxima')
  tail(labels, 1) == label
}
