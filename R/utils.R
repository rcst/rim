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
      promptExpr <- "prompt;>>([[:space:]|[:print:]]*)<<prompt;"
      repeat {
	z <- readLines(con, n = 1, warn = FALSE)
	if(length(z)) {
	  private$reply <- c(private$reply, z)
	  if(grepl(pattern = promptExpr, x = z)) 
	    break
	}
      }

      # extract outs and betweens
      # outExpr <- "out;>>([[:space:]|[:print:]]*?)<<out;"
      nr <- length(private$reply)
      iouts <- which(regexpr("out;>>|<<out;", private$reply) == 1L)
      n <- length(iouts)

      if(n < 2L || n %% 2 != 0)
	stop("Could not fetch a result.")

      private$outs  <- private$betweens <- logical(nr)
      private$outs[iouts] <- TRUE

      private$betweens <- private$reply[!swipe(outs)]
      private$betweens[nr] <- FALSE
      private$betweens <- private$reply[private$betweens]

      private$outs <- private$reply[swipe(outs, FALSE)]

      private$promptID <- as.integer(regex(text = private$reply[nr], 
					   pattern = "\\(%i(\\d+)\\)")[2])
    }, 

  print = function(...) {
    cat("Reply object:\n")
    cat("  prompt ID:", private$promptID, "\n", sep = "")
    cat("  raw:", private$reply, "\n", sep = "")
    cat("  outs:", private$outs, "\n", sep = "")
    cat("  betweens:", private$betweens, "\n", sep = "")
  }),

  private = list( 
    reply = character(),
    prompt = character(),
    promptID = integer(),
    outs = list(),
    betweens = list()
  )
)

#' returns character vector of all matches
regex <- function(text, pattern) {
  r <- regexec(pattern, text)
  starts <- unlist(r)
  stops <- starts + unlist(lapply(X = promptMatch, 
				  FUN = attr, 
				  which = "match.length"))
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
