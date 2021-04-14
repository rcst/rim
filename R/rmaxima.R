#' Starting maxima
#' 
#' Calling this function starts maxima in a child process in the background and configures the display output of maxima
#' 
#' @return No return value
#' @family rmaxima functions
#' @export
maxima.start <- function(restart = FALSE) { 
	maxima$startMaxima(restart) 
}

#' Stoping maxima process
#' 
#' Calling this function stops the maxima child process and deletes the corresponding objects 
#' 
#' @return No return value
#' @family rmaxima functions
#' @export
maxima.stop <- function() {
	maxima$stopMaxima()
}

#' Execute maxima command and retrieve result
#' 
#' @param command A character vector containing the maxima command.
#' @return A character vector containing the corresponding output from maxima
#' @export
#' @examples
#' m <- new(RMaxima)
#'
#' maxima.tell("2+2")
#' maxima.tell("2+2;")
#' maxima.tell("jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] )")
maxima.tell <- function(command) maxima$execute(command) 

#' Let maxima load a module
#' 
#' @param module A character vector naming the maxima module to be loaded.
#' @return A character vector containing the corresponding output from maxima
#' @export
#' @examples
#' maxima.load("ctensor")
maxima.load <- function(module) maxima$loadModule(command) 

#' Uses maximas lookup function \code{apropos()} and retrieves result
#' 
#' @param command A character vector containing the maxima command.
#' @return A character vector containing the corresponding output from maxima
#' @export
#' @examples
#' maxima.start()
#'
#' maxima.apropos("int")
maxima.apropos <- function(keystring) maxima$apropos(keystring) 

#' Change LaTeX math environment delimiters of output
#' 
#' This function sets the math environment delimiters for the LaTeX output of maxima.
#' 
#' @param env A character vector containing the maxima command.
#' @param label A character vector. If set, assigns label \code{label} to the environment of the output where suitable, i.e. when \code{env} is "equation". This is useful in case the output is used for a LaTeX document so that the equation can be referenced throughout the document by \code{eqref} 
#' @return A character vector containing the corresponding output from maxima
#' @export
#' @examples
#' maxima.settexenv("equation", "foo")
maxima.settexenv <- function(env, label) {
	if(!is.character(env))
		stop("Invalid input: expected character vector")

	switch(env,
	       math = maxima$setTexEnv("$", 
				       "$"),
	       displaymath = maxima$setTexEnv("\\begin{displaymath}", 
					      "\\end{displaymath}"),
	       equation = {
		       if(missing(label)) 
			       maxima$setTexEnv("\\begin{equation}", 
						"\\end{equation}") 
		       else 
			       maxima$setTexEnv(paste0("\\begin{equation}\\label{", 
						       label, 
						       "}"), 
						"\\end{equation}")
	       },
	       default = maxima$setTexEnv("","")
	       )
}
