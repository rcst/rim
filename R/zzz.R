.onLoad <- function(libname, pkgname) {
  maxima.env$maxima <- RMaxima$new()
}

.onAttach <- function(libname, pkgname) {
  if(maxima.isInstalled()) { 
    maxima.start() 
    if(maxima.version() < "5.42.1") {
      packageStartupMessage(paste("Installed Maxima version ", maxima.version(), 
                                  "is untested. Consider updating.")) 
    }
    maxima.stop()
  }
  else {
    packageStartupMessage(paste("Could not find Maxima executable, please download from\n", 
                                "https://maxima.sourceforge.io/download.html\n",
                                "and install"))
    if(.Platform$OS != "unix") 
      packageStartupMessage(paste("Please make sure maxima executable", 
                                  "is on PATH environment variable."))
  }

  if(requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(maxima = maxima.engine)
    setup_hooks()
    packageStartupMessage("Maxima successfully registered as knitr engine!")
  } else
    packageStartupMessage("Install package knitr if you want to register maxima a knitr engine first")
}

.onUnload <- function (libpath) { 
  suppressMessages({
    library.dynam.unload("rim", libpath)
    rm(list = "maxima", envir = maxima.env)
    if(exists(x = "mx", envir = maxima.env))
      rm(list = "mx", envir = maxima.env)
  })
}

.onDetach <- function(libpath) {
  suppressMessages({
    maxima.env$maxima$stop()
    if(exists(x = "mx", envir = maxima.env))
      maxima.env$mx$stop()
  })
}
