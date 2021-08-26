#' maxima.options
#'
#' Function for globally setting various options.
#'
#' @param format Character vector of length 1.
#' @param engine.format ...
#' @param inline.format ...
#' @param label ...
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
