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

      # read socket until end of input 
      # reply <- readLines(con, warn = FALSE)
      # if(tail(reply, n = 1)) {
      #   stop("Read failed, was read: ", 
      #        paste0(reply, collapse = "\n"))
      # }

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
			  utilDir, 
			  display = "maxima-init-tex2", 
			  port = 27182) {
      if(missing(maximaPath))
	private$maximaPath <- Sys.which("maxima")
      if(missing(workDir))
	workDir = getwd()
      if(missing(utilDir))
	utilsDir <- normalizePath(dirname(system.file("extdata", 
						      "maxima-init-lin.mac", 
						      package = "rmaxima", 
						      mustWork = TRUE)))
      private$port <- port

      scon <- serverSocket(port)
      system2(maximaPath, 
	      c("-q", 
		paste0("-s ", port), 
		paste0("--userdir=", utilsDir), 
		paste0("--init=", display)), 
	      wait = FALSE, 
	      stdout = FALSE, 
	      stderr = stdout())

      private$maximaSocket <- socketAccept(socket = scon, 
					   blocking = FALSE, 
					   open = "r+b")
      close(scon)

      # read-out pid
      pid <- as.integer(regex(pattern = "pid=(\\d+)", 
			      readLines(con = private$maximaSocket, 
					n = 1, 
					warn = FALSE))[2])
      # flush
      Reply$new(private$maximaSocket)
    },
    finalize = function() {
      cat("Quitting Maxima\n")
      private$sendCommand("quit();")
      close(private$maximaSocket)
    },
    executeCommand = function(command){
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
    maximaPath = character(),
    display = character(),
    reply = NULL,
    readReply = function() {},

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
    },

    lastPromptID = integer(),
    lastInputLabel = character(),
    lastOutputLabel = character()
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

