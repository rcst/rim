setup_hooks <- function() {
  old_output <- knitr::knit_hooks$get("output")
  knitr::knit_hooks$set(output = function(x, options) {
			  old_output(x, options)
})


#   knitr::knit_hooks$set(maxima.format = function(before, options) {
# 			  # this functions will print it's return value
# 			  # i.e. the last statement if it results in a character vector
# 			  if(options$engine == "maxima") {
# 			    if(before) {
# 			      maxima.engine.start()
# 			      # linear is default anyway
# 			      if(options$maxima.format != "linear") {
# 				maxima.env$engine_format <- switch_format(maxima.env$mx, 
# 									  options$maxima.format)
# 			      }
# 			    }
# 			    else {
# 			      # engine has been stopped after last chunk, cannot switch
# 			      if(!last_label()) {
# 				maxima.env$engine_format <- switch_format(maxima.env$mx) 
# 			      }
# 			    } 
# 			    return()
# 			  }
# })

#   knitr::knit_hooks$set(maxima.labels = function(before, options) {
# 			  if(is.logical(options$maxima.labels) && options$engine == "maxima") {
# 			    if(before) {
# 			      maxima.engine.start() 
# 			      maxima.env$engine_ref_labels <- options$maxima.labels
# 			    }
# 			    else {
# 			      # engine has been stopped after last chunk, cannot switch
# 			      if(!last_label()) {
# 				maxima.env$engine_ref_labels <- TRUE
# 			      }
# 			    } 
# 			    return()
# 			  }
# })
}
