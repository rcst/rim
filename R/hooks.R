setup_hooks <- function() {
  old_output <- knitr::knit_hooks$get("output")
  knitr::knit_hooks$set(output = function(x, options) old_output(x, options))
}
