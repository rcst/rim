switch_format <- function(format = "linear") {
  stopifnot(is.character(format))
  switch(format,
	 "latex" = format,
	 "linear" = format,
	 "ascii" = format,
	 "mathml" = format,
	 format <- "linear"
  )
  return(format)
}
