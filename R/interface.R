#' @import R6
MReader <- R6::R6Class("MReader",
  public = list(
    initialize = function(con) {
      private$rc <- con
      private$stash <- ""
      private$complete <- TRUE
      private$Nnon <- 0L
    },
    wasComplete = function() {
      private$complete
    }
  ),
  active = list(
    trials = function(value) {
      if (missing(value)) {
        private$Nnon
      } else {
        private$Nnon <- value
      }
    },
    read = function(value) {
      if (!missing(value)) {
        message("Input value ignored.")
      }
      private$complete <- TRUE
      suppressWarnings(withCallingHandlers(
        {
          x <- readLines(private$rc, n = 1)
          z <- paste0(private$stash, x)
        },
        warning = function(cnd) {
          private$complete <- FALSE
        }
      ))
      if (private$complete & length(z)) {
        private$stash <- ""
        private$Nnon <- 0L
      } else {
        private$stash <- z
        private$complete <- FALSE
        private$Nnon <- private$Nnon + 1L
      }

      return(z)
    }
  ),
  private = list(
    stash = character(0),
    complete = logical(0),
    rc = NULL,
    Nnon = integer(0)
  )
)

Reply <- R6::R6Class("Reply",
  public = list(
    initialize = function(con) {
      if (missing(con)) {
        stop("Please provide a connection object to initialize.")
      }

      if (!(isOpen(con, rw = "read") && isOpen(con, rw = "write"))) {
        stop("Connection without read/write access")
      }

      # read socket until including prompt
      promptExpr <- "<<prompt;"
      promptDefault <- "^\\(%i\\d+\\) $"
      promptHit <- FALSE
      rdr <- MReader$new(con)
      repeat {
        repeat {
          z <- rdr$read
          if (grepl(pattern = promptExpr, x = z)) {
            promptHit <- TRUE
            break
          } else if (grepl(pattern = promptDefault, x = z)) {
            if (rdr$wasComplete()) break else next
          }
          if (rdr$wasComplete()) break
        }
        if (nchar(z)) {
          private$reply <- c(private$reply, z)
          if (promptHit) {
            break
          }
        }
      }

      # extract prompt, outs and betweens
      # outExpr <- "out;>>([[:space:]|[:print:]]*?)<<out;"
      nr <- length(private$reply)

      # prompt
      iprompt <- grep(pattern = "prompt;>>|<<prompt;", x = private$reply)
      iprompt <- pvseq(iprompt)
      private$prompt <- private$reply[iprompt]
      private$outs <- extract("out;>>|<<out;", private$reply, TRUE)

      if (length(attr(private$outs, "delimiters")) %% 2 != 0) {
        stop(paste(
          "Could not fetch a output:",
          paste0(private$reply[-iprompts], collapse = "\n")
        ))
      }
      private$outputLabel <- extract("lab;>>|<<lab;", private$outs)

      # get output label from linear output
      if (!length(private$outputLabel)) {
        private$outputLabel <- NA_character_
      }

      # betweens
      private$betweens <- private$reply[-c(
        iprompt,
        attr(private$outs, "all")
      )]

      # get prompt ID/ label
      promptMatch <- regex(
        text = paste0(private$prompt, collapse = "\n"),
        pattern = "\\((%i(\\d+))\\)"
      )

      if (length(promptMatch)) {
        private$validPrompt <- TRUE
        private$inputLabel <- promptMatch[2]
        private$promptID <- as.integer(promptMatch[3])
      } else {
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
      any(grepl(
        pattern = "Console interrupt|User break|Interactive interrupt",
        x = private$betweens
      ))
    },
    hasWarning = function() {
      any(grepl(pattern = "warning|converting float", ignore.case = TRUE, x = private$betweens))
    },
    requireUser = function() {
      any(grepl(
        pattern = "TEXT;>>|<<TEXT;",
        x = private$prompt
      ))
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
      # private$result
    }
  ),
  private = list(
    reply = character(),
    prompt = character(),
    promptID = integer(),
    inputLabel = character(),
    outputLabel = character(),
    outs = character(),
    betweens = character(),
    validPrompt = logical(),
    dollar = logical()
  ),
  active = list(
    suppressed = function(value) {
      if (missing(value)) {
        private$dollar
      } else if (is.logical(value)) {
        private$dollar <- value
      } else {
        stop("Expected logical value")
      }
    },
    result = function(value) {
      if (!missing(value)) {
        stop("'result' is read-only")
      }
      if (self$suppressed) {
        return(list())
      } else {
        wtl <- extract("wtl;>>|<<wtl;", private$outs)
        wol <- extract("wol;>>|<<wol;", private$outs)
        return(list(
          "wtl" = list(
            "linear" = extract("lin;>>|<<lin;", wtl),
            "ascii" = extract("two;>>|<<two;", wtl),
            "latex" = extract("tex;>>|<<tex;", wtl),
            "inline" = extract("tin;>>|<<tin;", wtl),
            "mathml" = extract("htm;>>|<<htm;", wtl)
          ),
          "wol" = list(
            "linear" = extract("lin;>>|<<lin;", wol),
            "ascii" = extract("two;>>|<<two;", wol),
            "latex" = extract("tex;>>|<<tex;", wol),
            "inline" = extract("tin;>>|<<tin;", wol),
            "mathml" = extract("htm;>>|<<htm;", wol),
            "rstr" = extract("m2r;>>|<<m2r;", wol)
          )
        ))
      }
    }
  )
)

RMaxima <- R6::R6Class("RMaxima",
  public = list(
    initialize = function(
      maximaPath = "maxima", 
      workDir, 
      display = "display", 
      port = 27182) {
      if (missing(maximaPath)) {
        if (is.na(private$maximaPath <- Sys.getenv("RIM_MAXIMA_PATH", unset = NA))) {
          private$maximaPath <- Sys.which("maxima")
        }
      } else {
        private$maximaPath <- maximaPath
      }

      if (missing(workDir)) {
        private$workDir <- getwd()
      } else {
        private$workDir
      }

      private$macInitFile <- system.file(
        "extdata",
        "display.mac",
        package = "rim",
        mustWork = TRUE
      )

      private$lispInitFile <- system.file(
        "extdata",
        "display.lisp",
        package = "rim",
        mustWork = TRUE
      )

      private$display <- display
      private$port <- port
    },
    isInstalled = function() {
      if (!nchar(private$maximaPath) ||
        length(grep(
          pattern = "maxima",
          x = private$maximaPath,
          ignore.case = TRUE
        )) == 0L) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    },
    start = function(restart = FALSE) {
      if (!self$isInstalled()) {
        stop("Could not find maxima executable, please install first")
      } else {
        if (private$running) {
          if (restart) {
            self$stop()
            private$run()
          } else {
            message("Maxima is already running.")
          }
        } else {
          private$run()
        }
      }
    },
    stop = function() {
      if (private$running) {
        private$sendCommand("quit();")
        private$running <- FALSE
        close(private$maximaSocket)
      } else {
        message("Maxima is not running.")
      }
    },
    get = function(command) {
      if (!is.null(private$reply)) {
        if (!is.na(private$reply$getInputLabel())) {
          private$lastInputLabel <- private$reply$getInputLabel()
        }
      }

      if ((cmd <- trim(checkCommand(command))) == ";") {
        return(structure(
          list(),
          input.label = private$lastInputLabel,
          output.label = NA,
          command = command,
          suppressed = TRUE,
          parsed = NA,
          class = "maxima"
        ))
      }

      private$crudeExecute(cmd)

      if (private$reply$hasWarning()) {
        warning(private$reply$getBetweens())
      }

      if (private$reply$is.empty()) {
        if (private$reply$requireUser()) {
          return(regex(
            text = private$reply$getPrompt(),
            pattern = "TEXT;>>(.*)<<TEXT;"
          )[2])
        }

        if (!private$reply$checkPrompt()) {
          stop(paste("Unsupported.", gsub(
            pattern = "TEXT;>>|<<TEXT;",
            replacement = "",
            x = private$reply$getBetweens()
          )))
        }

        if (private$reply$isInterrupted()) {
          stop("Command execution was interrupted.")
        }

        # if it was a lisp command, we receive a plain string
        if (!(was_lisp <- grepl("^:lisp", trim(command)))) {
          # logic: it's an error, if it's not definitely a warning
          if (nchar(err <- private$reply$getBetweens()) &&
            !private$reply$hasWarning()) {
            stop(err)
          }
        }

        # conclusion: output must have been suppressed
        return(structure(private$reply$result,
          input.label = private$lastInputLabel,
          output.label = private$lastOutputLabel,
          command = command,
          # lisp = was_lisp,
          suppressed = private$reply$suppressed,
          parsed = NA,
          from_engine = FALSE,
          class = "maxima"
        ))
      }

      # forward any messages if no warning has been detected already
      if (nchar(private$reply$getBetweens()) &&
        !private$reply$hasWarning()) {
        message(private$reply$getBetweens())
      }

      # validate output and return if valid
      if (!is.na(private$reply$getOutputLabel())) {
        private$lastOutputLabel <- private$reply$getOutputLabel()

        if (any(grepl("no-convert", p <- private$reply$result$wol$rstr))) {
          warning(
            "Couldn't parse non-suppressed Maxima output.",
            "\nIf you think it should be parsed, please submit an issue at",
            "\nhttps://github.com/rcst/rim/issues\n", p
          )
          p <- NA_character_
        }

        tryCatch(
          error = function(cnd) {
            warning(
              "Caught error while parsing\n",
              cnd$message,
              "\nReturning NA."
            )
            p <- NA
          },
          p <- str2lang(p)
        )

        return(structure(private$reply$result,
          input.label = private$lastInputLabel,
          output.label = private$lastOutputLabel,
          command = command,
          suppressed = private$reply$suppressed,
          parsed = p,
          from_engine = FALSE,
          class = "maxima"
        ))
      }

      # private$crudeExecute(";")
      stop(paste("Unsupported:", private$reply$concatenateParts()))
    },
    getLastPromptID = function() {
      private$lastPromptID
    },
    getCurrentInputLabel = function() {
      private$reply$getInputLabel()
    },
    getLastInputLabel = function() {
      private$lastInputLabel
    },
    getLastOutputLabel = function() {
      private$lastOutputLabel
    },
    getLisp = function(command) {
      stopifnot(
        is.character(command),
        length(command) == 1
      )

      command <- paste(":lisp", command)
      private$crudeExecute(command)
      private$reply$getBetweens()
    },
    loadLispFile = function(file) {
      stopifnot(
        is.character(file),
        length(file) == 1,
        nchar(file) > 0
      )

      file <- system.file(
        "extdata",
        paste0(file, ".lisp"),
        package = "rim",
        mustWork = TRUE
      )

      command <- paste0("(load \"", file, "\")")

      self$getLisp(command)
    },
    loadLib = function(lib, quote = FALSE, stuck = TRUE, suppress = TRUE) {
      stopifnot(
        is.character(lib), 
        is.logical(quote), 
        is.logical(stuck),
        is.logical(suppress)
      )

      if (length(lib) > 0 && nchar(lib)) {
        lib <- trim(lib)

        is_quoted <- substr(lib, 1, 1) == "\"" & 
          substr(lib, nchar(lib), nchar(lib)) == "\""

        if(quote & !(is_quoted)) {
          lib <- paste0("\"", lib, "\"")
        }

        cmd <- paste0("load(", lib, ")")
        cmd <- paste0(cmd, ifelse(suppress, "$", ";")) 

        if (stuck) {
          self$getStuck(cmd)
        } else {
          self$get(cmd)
        }
      }
    },
    loadPackageFile = function(file, ...) {
      stopifnot(
        is.character(file),
        length(file) == 1,
        nchar(file) > 0
      )
      file <- system.file(
        "extdata",
        file,
        package = "rim",
        mustWork = TRUE
      )
      self$loadLib(lib = file, ...)
    },
    getStuck = function(command) {
      # executes command without advancing reference labels
      command <- trim(checkCommand(command))
      suppressed <- ifelse((terminator <- substring(command, first = nchar(command))) == ";", FALSE, TRUE)
      command <- substring(command, 1, nchar(command)-1L)

      if (length(command) && nchar(command)) {
        self$get(paste0("(linenum:linenum-1, ", command, ")", terminator))
      }
    },
    getPort = function() {
      if (private$running) {
        return(private$port)
      } else {
        return(NA_integer_)
      }
    },
    getVersion = function() {
      if (private$running) {
        return(private$version)
      } else {
        message("Version number undetermined. Maxima needs to be running.")
        return(NULL)
      }
    },
    getLispBackend = function() {
      if (private$running) {
        return(private$lispBackend)
      } else {
        message("Version number undetermined. Maxima needs to be running.")
        return(NULL)
      }
    }
  ),
  private = list(
    maximaSocket = NULL,
    port = NULL,
    pid = NULL,
    version = NULL,
    lispBackend = NULL,
    workDir = character(),
    macInitFile = character(),
    lispInitFile = character(),
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
      for (port in private$port:65536) {
        try(scon <- serverSocket(port), silent = TRUE)
        if (exists("scon")) {
          if (isOpen(con = scon)) {
            private$port <- port
            break
          }
        }
      }
      if (!exists("scon")) {
        stop("Couldn't find available port")
      }

      system2(private$maximaPath,
        c( # "-q",
          paste0("-s ", private$port),
          paste0("--init-mac=", private$macInitFile),
          paste0("--init-lisp=", private$lispInitFile)
        ),
        # paste0(private$preload)),
        wait = FALSE,
        stdout = FALSE,
        stderr = FALSE
      )
      # args <- c(
      #   paste0("-s ", private$port),
      #   paste0("--init-mac=", private$macInitFile),
      #   paste0("--init-lisp=", private$lispInitFile)
      # )

      # system2("/bin/sh",
      #   c("-c", shQuote(sprintf("exec %s %s >/dev/null 2>&1",
      #     private$maximaPath,
      #     paste(args, collapse = " ")))),
      #   wait = FALSE, stdout = FALSE, stderr = FALSE)

      if (!socketSelect(socklist = list(scon), timeout = 10)) {
        stop("Maxima did not connect within timeout")
      }

      private$maximaSocket <- socketAccept(socket = scon, blocking = FALSE, open = "r+b")

      close(scon)

      private$parseStartUp()
      private$reply <- Reply$new(private$maximaSocket)
      private$lastInputLabel <- private$reply$getInputLabel()
      private$running <- TRUE
      private$lispBackend <- private$readLispBackend()
    },
    sendCommand = function(command) {
      if (!private$running) self$start()
      if (missing(command)) stop("Missing command.")

      writeLines(text = command, con = private$maximaSocket)
      return(command)
    },
    crudeExecute = function(command) {
      command <- private$sendCommand(command)
      private$reply <- Reply$new(private$maximaSocket)
      if (grepl("\\$$", command)) {
        private$reply$suppressed <- TRUE
      }
    },
    parseStartUp = function(timeout = 30) {
      # pid
      pidExpr <- "pid=(\\d+)"
      deadline <- Sys.time() + timeout
      rdr <- MReader$new(private$maximaSocket)
      repeat {
        z <- rdr$read
        if (length(z)) {
          if (grepl(pattern = pidExpr, x = z)) {
            private$pid <- as.integer(regex(text = z, pattern = pidExpr)[2])
            break
          }
        }
        if (Sys.time() > deadline) {
          stop("Failed to read process ID - couldn't start Maxima.")
        }
      }

      # version
      verExpr <- "Maxima ((\\d+\\.)?(\\d+\\.)?(\\d+))"
      deadline <- Sys.time() + timeout
      Nparsed <- 0L
      seen <- character()
      repeat {
        z <- rdr$read
        if (length(z) && nchar(z)) {
          seen <- c(seen, z)
          if (grepl(pattern = verExpr, x = z)) {
            private$version <- numeric_version(regex(text = z, pattern = verExpr)[2])
            break
          }
        }
        Nparsed <- Nparsed + 1L
        if (Sys.time() > deadline) {
          stop(
            "Maxima stuck reading version number\n",
            "Maxima call: ", paste0(
              private$maximaPath, " -s ", private$port,
              " --init-mac=", private$macInitFile,
              " --init-lisp=", private$lispInitFile,
              " complete=", rdr$wasComplete(),
              " nlines=", length(seen),
              " raw:\n", paste(seen, collapse = "\n")
            )
          )
        }
      }
    },
    readLispBackend = function() {
      self$getLisp("(format nil \"~a~%\" (lisp-implementation-type))")
    },
    finalize = function() {
      # in here no more method calls are possible
      # suppressMessages(self$stop())
      if (private$running) {
        private$sendCommand("quit();")
        close(private$maximaSocket)
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
  stops <- starts + unlist(lapply(
    X = r,
    FUN = attr,
    which = "match.length"
  )) - 1L
  substring(text, starts, stops)
}

swipe <- function(x, inclusive = TRUE) {
  stopifnot(is.logical(x))
  ix <- which(x)
  r <- cumsum(x) %% 2L
  r[x] <- FALSE
  if (inclusive) {
    r[x] <- TRUE
  }
  as.logical(r)
}

odd <- function(x) x %% 2 != 0

vseq <- function(from, to, unique = TRUE) {
  if (unique) {
    unique(unlist(Map(":", from, to)))
  } else {
    unlist(Map(":", from, to))
  }
}

pvseq <- function(x) {
  if (length(x) >= 2 && length(x) %% 2 == 0) {
    vseq(
      from = x[odd(1:length(x))],
      to = x[!odd(1:length(x))]
    )
  } else {
    x
  }
}

#' Extract elements from character vector 'from' by specifiying delimiters as a single regular expression.
#' @param delimiters Character vector specifying the delimiters that fence the elements to be extracted
#' @param from Character vector of length 1 containing the regular expression to be matched
#' @param inform logical of length 1. Whether information about the extraction indices should be set as attributes to the return value (TRUE), or not (FALSE, default).
#' @param without logical of length 1. Should the delimiters be excluded from the output (default)?
#' @return character vector with the elements extracted. Possibly empty, in case delimiters did not match
#' @noRd
extract <- function(delimiters, from, inform = FALSE, without = TRUE) {
  if (missing(delimiters)) {
    stop("Parameter 'delimiters' not provided")
  }
  if (length(delimiters) > 2L) {
    warning("Length of 'delimiters > 2, only the first two will be used.")
  }
  if (length(delimiters) == 1L) {
    delimiters <- c(delimiters, delimiters)
  }
  if (missing(from)) {
    stop("Parameter 'from' not provided")
  }

  i <- c(grep(pattern = delimiters[1], from), grep(pattern = delimiters[2], from))
  if (length(i) %% 2 != 0) {
    stop(paste(
      "Could not fetch delimiter pair:",
      paste0(from, collapse = "\n")
    ))
  }
  ii <- pvseq(i)
  ee <- ii[!(ii %in% i)]
  if (without) {
    r <- from[ee]
  } else {
    r <- from[ii]
  }

  if (inform) {
    attr(r, "all") <- ii
    attr(r, "elements") <- ee
    attr(r, "delimiters") <- i
    attr(r, "without") <- without
  }

  return(r)
}
