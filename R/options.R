#' maxima.options
#'
#' Function for globally setting and retrieving options.
#'
#' \describe{
#'   \item{\code{format}: }{character vector of length 1 setting the output format for \code{maxima.get()}. Can be one of \code{"linear",} \code{"ascii",} \code{"latex"} or \code{"mathml"}.}
#'   \item{\code{engine.format}: }{same as option \code{format}, but for outputs in \code{RMarkdown} documents.}
#'   \item{\code{inline.format}: }{character string setting the output format for \code{maxima.inline()}, for knitting outputs inline into \code{RMarkdown} documents. Can be one of \code{"linear"}, \code{"latex"} or \code{"mathml"}, but \emph{not} \code{"ascii"}.}
#'   \item{\code{label}: }{logical of length 1, whether reference labels should be printed for returned S3 objects from \code{\link{maxima.get}()} (TRUE, default), or not (FALSE). This also applies to printing of input commands using \code{\link{iprint}()}.}
#'   \item{\code{engine.label}: }{same as \code{label}, but for outputs in \code{RMarkdown} documents.}
#'   \item{\code{inline.label}: }{same as \code{label}, but for inline outputs in \code{RMarkdown} documents.}
#' }
# #'
# #' To print a table of available options, current settings and a description simply print the function object (without parentheses), i.e. \code{maxima.options} or \code{print(maxima.options)}.
# #'
#' @param ... options to be accessed by using \code{name = value}. Options can be added by setting \code{ADD = TRUE}, but only those mentioned below will be used my rim. 
#' @param RESET logical of length 1, whether to reset all options to default values.
#' @param READ.ONLY logical of length 1, can be used to output all options that are read-only.
#' @param LOCAL logical of length 1, to output all options that are defined locally. 
#' @param ADD logical of length 1, whether to add the specified option in \code{...} (TRUE), default is FALSE. 
# 
#'
#' @import GlobalOptions
#' @export
#' @examples
#' maxima.options(format = "latex")
#' maxima.options(label = FALSE)
#' maxima.options(label = TRUE, format = "ascii")
#' # reset to default
#' maxima.options(label = TRUE, format = "linear")
maxima.options = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}
maxima.options = set_opt(
  format = list(.value = "linear",
		.length = 1L,
		.class = "character", 
		.read.only = FALSE, 
		.validate = function(x) x %in% c("linear", "ascii", "latex", "mathml"),
		.failed_msg = "'format' must be one of 'linear', 'ascii', 'latex' or 'mathml'",
		.description = "Printing format of returned object from maxima.get()"),
  engine.format = list(.value = "linear", 
		       .length = 1L, 
		       .class = "character", 
		       .read.only = FALSE, 
		       .validate = function(x) {
			# add side effect: knitr::opts_chunk$set(result = "markup"/ "asis")
			 r <- x %in% c("linear", "ascii", "latex", "mathml")
			 # if(r) {
			 #   switch(x,
			 #          latex = knitr::opts_chunk$set(results = "asis"),
			 #          linear = knitr::opts_chunk$set(results = "markup"),
			 #          ascii = knitr::opts_chunk$set(results = "markup"),
			 #          mathml =  knitr::opts_chunk$set(results = "asis"),
			 #   )
			 # }
			 return(r)
		       }, 
		       .failed_msg = "'format' must be one of 'linear', 'ascii', 'latex' or 'mathml'",
		       .description = "Same as 'format', but for maxima code chunks in 'RMarkdown' documents."),
  engine.results = list(.value = function() {
			  switch(.v$engine.format,
				 latex = "asis",
				 linear = "markup",
				 ascii = "markup",
				 mathml = "asis",
			  )
		       }, 
		       .class = "character",
		       .length = 1L,
		       .private = TRUE, 
		       .visible = FALSE),
  inline.format = list(.value = "linear", 
		       .length = 1L, 
		       .class = "character", 
		       .read.only = FALSE, 
		       .validate = function(x) x %in% c("linear", "inline", "latex", "mathml"), 
		       .filter = function(x) ifelse(x == "latex", "inline", x),
		       .failed_msg = "'format' must be one of 'linear', 'inline' or 'mathml'",
		       .description = "Same as 'engine.format', but for printing output inline via maxima.inline(). Cannot be set to 'ascii'."),
  label = list(.value = TRUE,
	       .length = 1L,
	       .class = "logical",
	       .read.only = FALSE,
	       .description = "Sets whether a maxima reference label should be printed  when printing a maxima return object."),
  engine.label = list(.value = TRUE,
		      .length = 1L,
		      .class = "logical",
		      .read.only = FALSE,
		      .description = "Same as 'label', but for maxima code chunks."),
  inline.label = list(.value = TRUE,
		      .length = 1L,
		      .class = "logical",
		      .read.only = FALSE,
		      .description = "Same as 'label', but for inline code chunks"),
  display = list(.value = "display",
		 .length = 1L,
		 .class = "character",
		 .private = TRUE, 
		 .visible = FALSE),
  preload = list(.value = c("plot-knitr-png", "plot-knitr-pdf"),
		 .length = 1L,
		 .class = "character",
		 .private = TRUE, 
		 .visible = FALSE,
		 .description = "Specifies a file name that will be loaded after initialization to overwrite plotting function to include graphics into 'RMarkdown' documents."))
