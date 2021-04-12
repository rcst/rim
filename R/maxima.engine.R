maxima.env <- new.env()

#' knitr maxima engine
#'
#' An R-function that is registered as a knitr engine when package \code{rmaxima} is attached, i.e. \code{library(rmaxima)}. 
#'
#' \code{maxima.engine} is called by \code{knit()} to evaluate maxima code chunks. When called upon the first code chunk of a document it creates an object of reference class \code{RMaxima} in \code{knit()}'s execution environment. This spawns a child process running Maxima in the background. This means that a single Maxima session is used for all Maxima code chunks of an RMarkdown document. In detail, \code{maxima.engine} first checks whether an object named "mx" of type reference class exists. If not then it is created and hence Maxima is started in the background. Each line of code chunk is then turned into an unevaluated expression by using \code{call(name = mx$execute, code)}. When the last code chunk with option \code{engine = "maxima"} has been processed, \code{mx} is deleted and the Maxima child process quits.
#' 
#' @import knitr
#' @param options A list of chunk options
#' @return This functions prints the resulting output from maxima together with it's code 
maxima.engine <- function(options) {
	if(!exists("mx", envir = maxima.env)) { 
		message("starting maxima")
		assign(x = "mx", value = new(rmaxima:::RMaxima), envir = maxima.env)
		assign(x = "execute", 
		       value = get("mx", envir = maxima.env)$execute, 
		       envir = maxima.env)
		assign(x = "stopMaxima", 
		       value = get("mx", envir = maxima.env)$stopMaxima, 
		       envir = maxima.env)
	}

	code <- options$code
	out <- character(0);
	for(i in 1:length(code))
		out <- c(out, eval(call("execute", code[i]), envir = maxima.env))


	if (last_label(options$label)) { 
		message("stopping maxima")
		eval(call("stopMaxima"), envir = maxima.env) 
		rm(mx, envir = maxima.env) 
	}

	engine_output(options, code, out)
}


last_label <- function(label = knitr::opts_current$get('label')) {
  if (knitr:::child_mode()) return(FALSE)
  labels <- knitr::all_labels(engine == 'maxima')
  tail(labels, 1) == label
}


maxima.engine.fragile <- function(options) { 
	e <- parent.frame(n = sys.parent() - 2)
	if(!exists("mx", envir = e)) { assign(x = "mx", value = new(rmaxima:::RMaxima), envir = e)
		assign(x = "execute", value = get("mx", envir = e)$execute, envir = e)
		assign(x = "stopMaxima", value = get("mx", envir = e)$stopMaxima, envir = e)

		# quit maxima "on.exit"ing of knit()
		# eval() doesn't work on "special primitive functions
		# do.call() does ... this may break in the future
		# see https://stat.ethz.ch/pipermail/r-devel/2013-November/067874.html
		do.call("on.exit", list(quote(stopMaxima()), 
					add = TRUE), envir = e)
	}

	code <- options$code
	out <- character(0);
	for(i in 1:length(code))
		out <- c(out, eval(call("execute", code[i]), envir = e))

	engine_output(options, code, out)
}

maxima.engine.dumb <- function(options) {
	code <- options$code
	out <- character(0);
	for(i in 1:length(code)) {
		out <- c(out, maxima.tell(code[i]))
		# out <- eval(call("execute", code[i]), envir = e)

	}

	engine_output(options, code, out)
}

