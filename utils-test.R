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

source("R/utils.R")
maxima <- MaximaChain$new()
maxima$executeCommand("2+2;")
maxima$executeCommand("L: sqrt(1 - 1/R^2);")
maxima$executeCommand("integrate(x, x, 1, L);")
maxima$executeCommand("p;")
maxima$executeCommand("integrate(1/x, x, 1, L);")
maxima$executeCommand("p;")
rm(maxima)
