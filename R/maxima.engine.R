#' knitr maxima engine
#'
#' An R-function that is registered as a knitr engine when package \code{rmaxima} is attached, i.e. \code{library(rmaxima)}. 
#'
#' \code{maxima.engine} is called by \code{knit()} to evaluate maxima code chunks. When called upon the first code chunk of a document it creates an object of reference class \code{RMaxima} in \code{knit()}'s execution environment. This spawns a child process running Maxima in the background. This means that a single Maxima session is used for all Maxima code chunks of an RMarkdown document. In detail, \code{maxima.engine} captures \code{parent.frame()} and first checks whether an object named "mx" of type reference class exists there. If not then it is created and hence Maxima is started in the background. Each line of code chunk is then turned into an unevaluated expression by using \code{call(name = mx$execute, code)}. The code is then evaluated inside \code{knit()}'s execution environment by calling \code{eval(..., envir = parent.frame())}. When \code{knit()} exits \code{mx} is deleted and Maxima child process quits.
#' 
#' @import knitr
#' @param options A list of chunk options
#' @return This functions prints the resulting output from maxima together with it's code 
maxima.engine <- function(options) { 
	e <- parent.frame(n = sys.parent() - 2)
	if(!exists("mx", envir = e)) {
		message("starting maxima on first chunk")
		assign(x = "mx", value = new(rmaxima:::RMaxima), envir = e)
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

