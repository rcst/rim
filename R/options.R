#' maxima.options
#'
#' Function for globally setting and retrieving options.
#'
#' To print out a table of available options, current settings and a description simply print the function object (without parentheses), i.e. \code{maxima.options} or \code{print(maxima.options)}.
#'
#' @param format Character vector of length 1 setting the output format for \code{maxima.get()}. Can be one of "linear", "ascii", "latex" or "mathml".
#' @param engine.format Same as option \code{format}, but for outputs in RMarkdown documents.
#' @param inline.format Character vector of length 1 setting the output format for \code{maxima.inline()}, for knitting outputs inline into RMarkdown documents. Can be one of "linear", "latex" or "mathml". Note: Not "ascii".
#' @param label Logical of length 1, whether output reference labels should be printed for returned S3 objects from \code{maxima.get()} (TRUE, default), or not (FALSE).
#' @param engine.label Same as \code{label}, but for outputs in RMarkdown documents.
#' @param inline.label Same as \code{label}, but for inline outputs in RMarkdown documents.
#'
#' @import GlobalOptions
#' @export
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
			 if(r) {
			   switch(x,
				  latex = knitr::opts_chunk$set(results = "asis"),
				  linear = knitr::opts_chunk$set(results = "markup"),
				  ascii = knitr::opts_chunk$set(results = "markup"),
				  mathml =  knitr::opts_chunk$set(results = "asis"),
			   )
			 }
			 return(r)
		       }, 
		       .failed_msg = "'format' must be one of 'linear', 'ascii', 'latex' or 'mathml'",
		       .description = "Same as 'format', but for maxima code chunks in RMarkdown documents."),
  inline.format = list(.value = "linear", 
		       .length = 1L, 
		       .class = "character", 
		       .read.only = FALSE, 
		       .validate = function(x) x %in% c("linear", "inline", "mathml"), 
		       .failed_msg = "'format' must be one of 'linear', 'inline' or 'mathml'",
		       .description = "Same as 'label', but for printing output inline via maxima.inline()"),
  label = list(.value = TRUE,
	       .length = 1L,
	       .class = "logical",
	       .read.only = FALSE,
	       .description = "Sets whether a maxima reference output label should be printed  when printing a maxima return object."),
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
		 .read.only = TRUE)
)
