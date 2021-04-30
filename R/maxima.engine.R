#' knitr maxima engine
#'
#' An R-function that is registered as a knitr engine when package \code{rmaxima} is attached, i.e. \code{library(rmaxima)}. 
#'
#' \code{maxima.engine} is called by \code{knit()} to evaluate maxima code chunks. When called upon the first code chunk of a document it spawns a child process running Maxima in the background. This means that a single Maxima session is used for all Maxima code chunks of an RMarkdown document. 
#'
#' In addition, this function sets up Maxima specific output and chunk hooks.
#' 
#' @import knitr
#' @importFrom utils tail
#' @param options A list of chunk options
#' @return This functions prints the resulting output from maxima together with it's code 
maxima.engine <- function(options) { 
  maxima.engine.start()

  code <- options$code
  code <- collect_ends(code)
  out <- character(0);
  for(i in 1:length(code)) {
    tt <- maxima.env$mx$execute(code[i], maxima.env$engine_ref_labels)

    if(maxima.env$engine_format == "text2d") { 
      tt <- maxima.env$mx$execute(code[i], FALSE)
      if(!maxima.env$engine_ref_labels)
	 tt <- str_strip_col(x = tt, n = label_length(tt), side = "left") 
    }
    else 
      tt <- maxima.env$mx$execute(code[i], maxima.env$engine_ref_labels)
    
    out <- c(out, tt) 
  }

  if (last_label(options$label)) { 
    maxima.engine.stop()
  }

  engine_output(options, options$code, out)
}

maxima.engine.start <- function() {
  if(!exists("mx", envir = maxima.env)) { 
    maxima.env$mx <- new(RMaxima)
    maxima.env$engine_format <- "linear"
    maxima.env$engine_ref_labels <- TRUE
    setup_hooks()
  }
}

maxima.engine.stop <- function() { 
  maxima.env$mx$stopMaxima() 
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
