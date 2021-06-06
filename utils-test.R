source("R/utils.R")
# line read binary mode - at least 10 times faster than single character reading
scon <- serverSocket(port = 27182)
system2("maxima", 
	c("-q", 
	  "-s 27182", 
	  "--userdir=/home/eric/documents/R/packages/rmaxima/inst/extdata/", 
	  "--init=maxima-init-tex2"), 
	wait = FALSE, 
	stdout = FALSE, 
	stderr = FALSE)

con <- socketAccept(socket = scon, blocking = FALSE, open = "r+b")
close(scon)

get <- function() {
  z <- zz <- character(0)
  repeat {
    z <- readLines(con, n = 1, warn = FALSE)
    if(length(z)) {
      zz <- c(zz, z)
      if(grepl(pattern = "\\(%i[[:digit:]]+\\)", x = z)) 
	break
    }
  }
  zz
}

get()

writeLines("c: a^2 + b^2$", con = con)
r0 <- Reply$new(con)

writeLines("L: sqrt(1 - 1/R^2);", con = con)
r1 <- Reply$new(con)

writeLines("integrate(x, x, 1, L);", con = con)
r2 <- Reply$new(con)

writeLines("p;", con = con)
r3 <- Reply$new(con)

writeLines("integrate(1/x, x, 1, L);", con = con)
r4 <- Reply$new(con)

writeLines("p;", con = con)
r5 <- Reply$new(con)

writeLines(text = "quit();", con = con)
close(con)

library(R6)
library(Rcpp)
sourceCpp("src/utils.cpp")
source("R/utils.R")
maxima <- RMaxima$new()
maxima$loadInit("maxima-init-lin.mac")
maxima$loadInit("maxima-init-tex2.mac")
maxima$start()
maxima$start(TRUE)
maxima$get("c: sqrt(a^2 + b^2)$")
maxima$get("2+2;")
maxima$get("L: sqrt(1 - 1/R^2);")
maxima$get("integrate(x, x, 1, L);")
maxima$get("p;")
# maxima$get("integrate(1/x, x, 1, L);")
# maxima$get("p;")
maxima$stop()
maxima$stop()
maxima$start()
maxima$stop()
rm(maxima)
