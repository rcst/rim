#' knitr maxima engine
#'
#' An R-function that is registered as a knitr engine when package \code{rmaxima} is attached, i.e. \code{library(rmaxima)}. 
#'
#' \code{maxima.engine} is called by \code{knit()} to evaluate maxima code chunks. When called upon the first code chunk of a document it creates an object of reference class \code{RMaxima} in \code{knit()}'s execution environment. This spawns a child process running Maxima in the background. This means that a single Maxima session is used for all Maxima code chunks of an RMarkdown document. In detail, \code{maxima.engine} first checks whether an object named "mx" of type reference class exists. If not then it is created and hence Maxima is started in the background. Each line of code chunk is then turned into an unevaluated expression by using \code{call(name = mx$execute, code)}. When the last code chunk with option \code{engine = "maxima"} has been processed, \code{mx} is deleted and the Maxima child process quits.
#' 
#' @import knitr
#' @importFrom utils tail
#' @param options A list of chunk options
#' @return This functions prints the resulting output from maxima together with it's code 
maxima.engine <- function(options) {
	maxima.engine.start()

	code <- options$code
	out <- character(0);
	for(i in 1:length(code))
		out <- c(out, maxima.env$mx$execute(code[i]))


	if (last_label(options$label)) { 
		maxima.engine.stop()
	}


	engine_output(options, code, out)
}

maxima.engine.start <- function() {
	if(!exists("mx", envir = maxima.env)) { 
		maxima.env$mx <- new(RMaxima)
		maxima.env$engine_format <- "latex"
	}
}

maxima.engine.stop <- function() { 
	maxima.env$mx$stopMaxima() 
	rm(mx, engine_format, envir = maxima.env) 
}


last_label <- function(label = knitr::opts_current$get('label')) {
  # if (knitr:::child_mode()) return(FALSE)
  if(knitr::opts_knit$get('child')) return(FALSE)
  labels <- knitr::all_labels(engine == 'maxima')
  tail(labels, 1) == label
}


knitr::knit_hooks$set(maxima.format = function(before, options) {
	# this functions will print it's return value
	# i.e. the last statement if it results in a character vector
	if(before) {
		maxima.engine.start()
		if(options$maxima.format != maxima.env$engine_format) {
			maxima.env$engine_format <- switch_format(maxima.env$mx, 
									options$maxima.format)
			return()
		}
	}
	else {
		# engine has been stopped after last chunk
		if(!last_label()) {
			if((options$maxima.format != maxima.env$engine_format)) {
				maxima.env$engine_format <- switch_format(maxima.env$mx)
				return()
			}
		}
	} 
})
