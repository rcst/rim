#' @import R6
Reply <- R6::R6Class("Reply",  
  public = list(
    initialize = function(con) {
      if(missing(con)) 
	stop("Please provide a connection object to initialize.") 
      if(!(isOpen(con, rw = "read") && isOpen(con, rw = "write"))) 
	stop("Connection without read/write access")
      
      # read socket until including prompt 
      promptExpr <- "<<prompt;"
      repeat {
	z <- readLines(con, n = 1, warn = FALSE)
	if(length(z)) {
	  private$reply <- c(private$reply, z)
	  if(grepl(pattern = promptExpr, x = z)) 
	    break
	}
      }

      # extract prompt, outs and betweens
      # outExpr <- "out;>>([[:space:]|[:print:]]*?)<<out;"
      nr <- length(private$reply)

      # prompt
      iprompt <- grep(pattern = "prompt;>>|<<prompt;", x = private$reply)
      iprompt <- pvseq(iprompt)
      private$prompt <- private$reply[iprompt]

      # output
      iouts <- grep(pattern = "out;>>|<<out;", private$reply)
      if(length(iouts) %% 2 != 0)
	stop(paste("Could not fetch a output:", 
		   paste0(private$reply[-iprompts], collapse = "\n")))
      iouts <- pvseq(iouts)
      private$outs <- private$reply[iouts]

      # get output label
      outputMatch <- regex(text = paste0(private$outs, collapse = "\n"), 
			   pattern = "out;>>\\s+\\((%o(\\d+))\\)\\s\\n?([[:space:]|[:print:]]*)\\s+<<out;")
      if(length(outputMatch)) {
	private$outputLabel <- outputMatch[2]
	private$result <- outputMatch[4]
      }
      else
	private$outputLabel <- NA_character_

      # betweens
      private$betweens <- private$reply[-c(iprompt, iouts)]

      # get prompt ID/ label
      promptMatch <- regex(text = paste0(private$prompt, collapse = "\n"), 
			   pattern = "\\((%i(\\d+))\\)")

      if(length(promptMatch)) {
	private$validPrompt <- TRUE
	private$inputLabel <- promptMatch[2]
	private$promptID <- as.integer(promptMatch[3])
      }
      else {
	private$validPrompt <- FALSE
	private$inputLabel <- NA_character_
	private$promptID <- NA_integer_
      }

      private$dollar <- FALSE
    }, 
  print = function(...) {
    cat("Reply object:\n")
    cat("  prompt ID: ", private$promptID, "\n", sep = "")
    cat("  Input Label: ", private$inputLabel, "\n", sep = "")
    cat("  Output Label: ", private$outputLabel, "\n", sep = "")
    cat("  raw:", private$reply, "\n", sep = "")
    cat("  prompt: ", private$prompt, "\n", sep = "")
    cat("  outs: ", private$outs, "\n", sep = "")
    cat("  betweens: ", private$betweens, "\n", sep = "")
    cat("  result: ", private$result, "\n", sep = "")
    invisible(private$outs)
  },
  is.empty = function() {
    length(private$outs) == 0
  },
  checkPrompt = function() {
    private$validPrompt
  },
  getPromptID = function() {
    private$promptID
  },
  getInputLabel = function() {
    private$inputLabel
  },
  getOutputLabel = function() {
    private$outputLabel
  },
  isInterrupted = function() {
    any(grepl(pattern = "Console interrupt|User break|Interactive interrupt", 
	      x = private$betweens))
  },
  requireUser = function() {
    any(grepl(pattern = "TEXT;>>|<<TEXT;", 
	      x = private$prompt))
  },
  checkMaximaWarning = function() {
    any(grepl(pattern = "WARNING",
	      x = private$betweens))
  },
  checkMaximaError = function() {
    any(grepl(pattern = "^[[:space:]|[:print:]]{2,}$", 
	      x = private$betweens))
  },
  concatenateParts = function() {
    paste0(private$reply, collapse = "\n")
  },
  getPrompt = function() { 
    paste0(private$prompt, collapse = "\n")
  },
  getBetweens = function() { 
    paste0(private$betweens, collapse = "\n")
  },
  getOuts = function() {
    paste0(private$outs, collapse = "\n")
  },
  getResult = function() {
    private$result
  }),

  private = list( 
    reply = character(),
    prompt = character(),
    promptID = integer(),
    inputLabel = character(),
    outputLabel = character(),
    outs = character(),
    betweens = character(),
    result = character(),
    validPrompt = logical(),
    dollar = logical()
  ),
  active = list(
    suppressed = function(value) {
      if(missing(value))
	private$dollar
      else 
	if(is.logical(value))
	  private$dollar <- value
	else
	  stop("Expected logical value") 
    }
  )
)

RMaxima <- R6::R6Class("RMaxima",
  public = list(
    initialize = function(maximaPath = "maxima", 
			  workDir, 
			  utilsDir, 
			  display = "maxima-init-lin", 
			  port = 27182) {

      if(missing(maximaPath))
	private$maximaPath <- Sys.which("maxima")
      else
	private$maximaPath <- maximaPath

      if(missing(workDir))
	private$workDir = getwd()
      else
	private$workDir

      if(missing(utilsDir)) {
	private$utilsDir <- dirname(system.file("extdata", 
						      paste0(display, ".mac"), 
						      package = "rim", 
						      mustWork = TRUE))
	# for virtual machine package testing
	private$utilsDir <- gsub(pattern = "\\\\vboxsrv", 
				 replacement = "Z:", 
				 x = private$utilsDir, 
				 ignore.case = TRUE)
      }
      else
	private$utilsDir <- utilsDir

      private$display <- display
      private$port <- port
    },
    finalize = function() {
      # in here no more method calls are possible
      # suppressMessages(self$stop())
      if(private$running) { 
	private$sendCommand("quit();")
	close(private$maximaSocket)
      }
    },
    isInstalled = function() {
      if(!nchar(private$maximaPath) || 
	 !grep(pattern = "maxima", 
	       x = private$maximaPath, 
	       ignore.case = TRUE))
	return(FALSE)
      else
	return(TRUE)
    },
    start = function(restart = FALSE) {
      if(!self$isInstalled())
	stop("Could not find maxima executable, please install first")
      else {
	if(private$running) {
	  if(restart) {
	    self$stop()
	    private$run()
	  }
	  else
	    message("Maxima is already running.")
	}
	else {
	  private$run()
	}
      }
    },
    stop = function() {
      if(private$running) {
	private$sendCommand("quit();")
	private$running <- FALSE
	close(private$maximaSocket)
      }
      else
	message("Maxima is not running.")
    },
    get = function(command){
      if(!private$running)
	self$start()

      if(!is.na(private$reply$getInputLabel()))
	private$lastInputLabel <- private$reply$getInputLabel()

      private$crudeExecute(command)

      if(private$reply$is.empty()) {
	if(private$reply$requireUser()) {
	  return(regex(text = private$reply$getPrompt(), 
		      pattern = "TEXT;>>(.*)<<TEXT;")[2]) 
	}

	if(!private$reply$checkPrompt()) {
	   stop(paste("Unsupported.", gsub(pattern = "TEXT;>>|<<TEXT;", 
	 				     replacement = "", 
	 				     x = private$reply$getBetweens())))
	}

	if(private$reply$isInterrupted()) {
	  stop("Command execution was interrupted.")
	}

	if(private$reply$checkMaximaWarning()) {
	  w <- gsub(pattern = "TEXT;>>|<<TEXT;", 
		       replacement = "", 
		       x = private$reply$getBetweens())
	  warning(w)
	  return(structure("",
			   input.label = private$lastInputLabel,
			   output.label = private$lastOutputLabel, 
			   command = command, 
			   suppressed = private$reply$suppressed,
			   class = "maxima"
			   ))
	}

	if(private$reply$checkMaximaError()) {
	  stop(gsub(pattern = "TEXT;>>|<<TEXT;", 
		    replacement = "", 
		    x = private$reply$getBetweens())) 
	}

	return(structure("",
			 input.label = private$lastInputLabel,
			 output.label = private$lastOutputLabel, 
			 command = command, 
			 suppressed = private$reply$suppressed,
			 class = "maxima"
			 ))
      }

      # validate output and return if valid
      if(!is.na(private$reply$getOutputLabel())) {
	private$lastOutputLabel <- private$reply$getOutputLabel()
	# return(private$reply$getResult())
	return(structure(private$reply$getResult(),
			 input.label = private$lastInputLabel,
			 output.label = private$lastOutputLabel,
			 command = command,
			 suppressed = private$reply$suppressed,
			 class = "maxima"))

      }

      private$crudeExecute(";") 
      stop(paste("Unsupported:", private$reply$concatenateParts()))
    },
    getLastPromptID = function() {
      private$lastPromptID
    },
    getLastInputLabel = function() {
      private$lastInputLabel
    },
    getLastOutputLabel = function() {
      private$lastOutputLabel
    },
    loadModule = function(module) {
      if(length(module) > 0 && nchar(module))
	self$get(paste0("load(\"", module, "\")$")) 
    },
    loadInit = function(file) {
      if(length(file) > 0 && nchar(file) > 0) {
	self$get(paste0("(load(\"", private$utilsDir, "/", file, "\"),",
			"linnum:linenum-1, %);"))
      }

      # return NULL
      invisible()
    },
    getPort = function() {
      if(private$running) 
	return(private$port)
      else
	return(NA_integer_)
    },
    getVersion = function() {
      if(private$running)
	return(private$version)
      else
	return(NULL)
    }
    ),
  private = list(
    maximaSocket = NULL,
    port = NULL,
    pid = NULL,
    version = NULL,
    workDir = character(),
    utilsDir = character(),
    maximaPath = NA_character_,
    display = character(),
    reply = NULL,
    running = FALSE,
    lastPromptID = integer(),
    lastInputLabel = character(),
    lastOutputLabel = character(),
    run = function() {
      # try until free port is found
      # starting from given port
      for(port in private$port:65536) { 
	try(scon <- serverSocket(port), silent = TRUE)
	if(exists("scon")) 
	  if(isOpen(con = scon)) {
	    private$port <- port
	    break
	  }
      }
      if(!exists("scon"))
        stop("Couldn't find available port")

      system2(private$maximaPath, 
	      c(# "-q", 
		paste0("-s ", private$port), 
		paste0("--userdir=", private$utilsDir), 
		paste0("--init=", private$display)), 
	      wait = FALSE, 
	      stdout = FALSE, 
	      stderr = stdout())
      
      private$maximaSocket <- socketAccept(socket = scon, 
					   blocking = FALSE, 
					   open = "r+b")
      close(scon)

      private$parseStartUp()
      private$reply <- Reply$new(private$maximaSocket)
      private$lastInputLabel <- private$reply$getInputLabel
      private$running <- TRUE

    },
    sendCommand = function(command){
      if(missing(command))
	stop("Missing command.")

      command <- trim(command)
      command <- checkCommand(command)

      writeLines(text = command, con = private$maximaSocket)

      return(command)
    }, 
    crudeExecute = function(command) {
      command <- private$sendCommand(command)
      private$reply = Reply$new(private$maximaSocket)
      if(grepl("\\$$", command))
	private$reply$suppressed <- TRUE
    },
    parseStartUp = function() {
      # pid
      pidExpr <- "pid=(\\d+)"
      repeat {
	z <- readLines(private$maximaSocket, n = 1, warn = FALSE)
	if(length(z)) {
	  if(grepl(pattern = pidExpr, x = z)) { 
	    private$pid <- as.integer(regex(text = z, pattern = pidExpr)[2])
	    break
	  }
	}
      }

      # version
      verExpr <- "Maxima ((\\d+\\.)?(\\d+\\.)?(\\d+))"
      repeat {
	z <- readLines(private$maximaSocket, n = 1, warn = FALSE)
	if(length(z)) {
	  if(grepl(pattern = verExpr, x = z)) { 
	    private$version <- numeric_version(regex(text = z, pattern = verExpr)[2])
	    break
	  }
	}
      }
    }
  )
)

#' Extract substring by regular expression
#' @param text Character vector containing the text to be searched
#' @param pattern Character vector of length 1 containing the regular expression to be matched
#' @return Character vector of the same length as \code{text}
#' @noRd
regex <- function(text, pattern) {
  r <- regexec(pattern, text)
  starts <- unlist(r)
  starts[starts == -1] <- NA
  stops <- starts + unlist(lapply(X = r, 
				  FUN = attr, 
				  which = "match.length")) - 1L
  substring(text, starts, stops)
}

swipe = function(x, inclusive = TRUE) {
  stopifnot(is.logical(x))
  ix <- which(x)
  r <- cumsum(x) %% 2L 
  r[x] <- FALSE 
  if(inclusive)
    r[x] <- TRUE
  as.logical(r)
}

odd <- function(x) x%%2!=0

vseq <- function(from, to, unique = TRUE) {
  if(unique)
    unique(unlist(Map(':', from, to)))
  else
    unlist(Map(':', from, to))
}

pvseq <- function(x) {
  if(length(x) >= 2 && length(x) %% 2 == 0) {
    vseq(from = x[odd(1:length(x))],
	 to = x[!odd(1:length(x))])
  }
  else
    x
}
