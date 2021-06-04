#' defines R6-class equivalent of MaximChain::Reply
#' @field reply A character vector storing the raw reply string from maxima
#' up until and including the next prompt
#' @field outs A list of character vectors storing the content delimited 
#' by output delimters "out;>>" and "<<out;"
#' @field betweens A list of character vectors storing each the the content between delimiter ";TEXT>>" and "<<TEXT;" 
#' @import R6
Reply <- R6Class("Reply",  
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

      # betweens
      private$betweens <- private$reply[-c(iprompt, iouts)]

      # get prompt ID
      promptMatch <- regex(text = private$prompt, pattern = "\\(%i(\\d+)\\)")
      if(length(promptMatch)) { 
	private$validPrompt <- TRUE
	private$promptID <- as.integer(promptMatch[2])
      }
      else {
	private$validPrompt <- FALSE
	private$promptID <- NA_integer_
      }
    }, 
  print = function(...) {
    cat("Reply object:\n")
    cat("  prompt ID:", private$promptID, "\n", sep = "")
    cat("  raw:", private$reply, "\n", sep = "")
    cat("  prompt: ", private$prompt, "\n", sep = "")
    cat("  outs:", private$outs, "\n", sep = "")
    cat("  betweens:", private$betweens, "\n", sep = "")
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
  isInterrupted = function() {
    any(grepl(pattern = "Console interrupt|User break|Interactive interrupt", 
	      x = private$betweens))
  },
  requireUser = function() {
    any(grepl(pattern = "TEXT;>>|<<TEXT;", 
	      x = private$prompt))
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
  }
  ),

  private = list( 
    reply = character(),
    prompt = character(),
    promptID = integer(),
    outs = character(),
    betweens = character(),
    validPrompt = logical()
  )
)

MaximaChain <- R6Class("MaximaChain",
  public = list(
    initialize = function(maximaPath = "maxima", 
			  workDir, 
			  utilsDir, 
			  display = "maxima-init-tex2", 
			  port = 27182) {

      if(missing(maximaPath))
	private$maximaPath <- Sys.which("maxima")
      else
	private$maximaPath <- maximaPath

      if(missing(workDir))
	private$workDir = getwd()
      else
	private$workDir

      if(missing(utilsDir))
	private$utilsDir <- normalizePath(dirname(system.file("extdata", 
						      paste0(display, ".mac"), 
						      package = "rmaxima", 
						      mustWork = TRUE)))
      else
	private$utilsDir <- utilsDir

      private$display <- display
      private$port <- port

      self$start()
    },
    finalize = function() {
      # in here no more method calls are possible
      # suppressMessages(self$stop())
      if(private$running) { 
	private$sendCommand("quit();")
	close(private$maximaSocket)
      }
    },
    start = function(restart = FALSE) {
      if(nchar(private$maximaPath) == 0)
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
      private$crudeExecute(command)

      if(private$reply$is.empty()) {
	if(private$reply$requireUser()) {
	  return(regex(text = private$reply$getPrompt(), 
		      pattern = "TEXT;>>(.*)<<TEXT;")[2]) 
	}

	if(private$reply$checkPrompt()) {
	   stop(paste("Unsupported.", gsub(pattern = "TEXT;>>|<<TEXT;", 
	 				     replacement = "", 
	 				     x = private$reply$getBetweens())))
	}

	if(private$reply$isInterrupted()) {
	  stop("Command execution was interrupted.")
	}

	if(private$reply$checkMaximaError()) {
	  stop(gsub(pattern = "TEXT;>>|<<TEXT;", 
		    replacement = "", 
		    x = private$reply$getBetweens())) 
	}

      return("")
      }

      # validate output and return if valid
      if(length(r <- regex(patter = paste0("out;>>", 
					   "\\s*\\", 
					   "((%o\\d+)\\)", 
					   "\\s+", 
					   "([[:space:]|[:print:]]*)", 
					   "\\s+", 
					   "<<out;"), 
			   text = private$reply$getOuts())) == 3) { 
	private$lastOutputLabel <- r[2]
	return(r[3])
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
    }
    ),

  private = list(
    maximaSocket = NULL,
    port = NULL,
    pid = NULL,
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
	try(scon <- serverSocket(port))
	if(exists("scon")) 
	  if(isOpen(con = scon)) {
	    private$port <- port
	    break
	  }
      }

      system2(private$maximaPath, 
	      c("-q", 
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

      # read-out pid
      private$pid <- as.integer(regex(pattern = "pid=(\\d+)", 
			      readLines(con = private$maximaSocket, 
					n = 1, 
					warn = FALSE))[2])
      # flush
      Reply$new(private$maximaSocket)
      private$running <- TRUE
    },
    sendCommand = function(command){
      if(missing(command))
	stop("Missing command.")

      command <- trim_copy(command)
      command <- checkCommand(command)

      writeLines(text = command, con = private$maximaSocket)
    }, 
    crudeExecute = function(command) {
      private$sendCommand(command)
      private$reply = Reply$new(private$maximaSocket)
    }
  )
)

#' returns character vector of all matches
regex <- function(text, pattern) {
  r <- regexec(pattern, text)
  starts <- unlist(r)
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

#' paired vector sequence
pvseq <- function(x) {
  if(length(x) >= 2 && length(x) %% 2 == 0) {
    vseq(from = x[odd(1:length(x))],
	 to = x[!odd(1:length(x))])
  }
  else
    x
}

