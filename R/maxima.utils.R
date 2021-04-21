switch_format <- function(obj, format = "linear") {
  switch(format,
	 latex = obj$loadInit("maxima-init-tex2.mac"),
	 linear = obj$loadInit("maxima-init-lin.mac"),
	 mathml = obj$loadInit("maxima-init-mathml.mac"),
	 default = {
	   obj$loadInit("maxima-init-lin.mac")
	   format <- "linear"
	 })

  return(format)
}

