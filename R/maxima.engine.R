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
	if(!exists("mx", envir = maxima.env)) { 
		# assign(x = "mx", value = new(RMaxima), envir = maxima.env)
		maxima.env$mx <- new(RMaxima)
		# assign(x = "execute", 
		#        value = get("mx", envir = maxima.env)$execute, 
		#        envir = maxima.env)
		# assign(x = "stopMaxima", 
		#        value = get("mx", envir = maxima.env)$stopMaxima, 
		#        envir = maxima.env)
	}

	code <- options$code
	out <- character(0);
	for(i in 1:length(code))
		# out <- c(out, eval(call("execute", code[i]), envir = maxima.env))
		out <- c(out, maxima.env$mx$execute(code[i]))


	if (last_label(options$label)) { 
		# eval(call("stopMaxima"), envir = maxima.env) 
		maxima.env$mx$stopMaxima()
		rm(mx, envir = maxima.env) 
	}

	engine_output(options, code, out)
}


last_label <- function(label = knitr::opts_current$get('label')) {
  # if (knitr:::child_mode()) return(FALSE)
  if(knitr::opts_knit$get('child')) return(FALSE)
  labels <- knitr::all_labels(engine == 'maxima')
  tail(labels, 1) == label
}
