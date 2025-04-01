#' Cluster a code chunk into commands
#' @param code Character vector containing the code chunk, one line per element.
#' @return A \code{list} where each element holds the indices of \code{code} that make up one command, i.e. terminates with either ';' or '$'
#' @noRd
gather <- function(code) {
  hits <- grepl(pattern = ";|\\$", x = code)
  comments <- grepl(pattern = "^[[:space:]]*/\\*.*?\\*/[[:space:]]*$", x = code)
  hits <- hits | comments
  marks <- rev(cumsum(rev(hits)))
  marks[code == ""] <- NA
  sapply(
    X = unique(marks),
    FUN = function(um, m) which(um == m),
    m = marks,
    simplify = FALSE
  )
}

#' retry
#' @param fun Character vector containing the code chunk, one line per element.
#' @param ... Arguments to be passed on to \code{retry_expr} and \code{knitr::include_graphics}.
#' @return Same return value as function \code{retry_expr()}.
#' @noRd
retry <- function(fun, ...) {
  expr <- substitute(fun)
  retry_expr(expr, ...)
}

#' Retry sub-function - recursively run \code{expr} until success or final failure
#' @param expr Character vector containing the code chunk, one line per element.
#' @param max.attempts Maximum number of times \code{expr} will be evaluated.
#' @param sleep.seconds Number of seconds to wait after each attempt.
#' @return Either returns the result of evaluating \code{expr} or an error condition object.
#' @noRd
retry_expr <- function(expr, max.attempts = 3, sleep.seconds = 0.5) {
  tryCatch(
    eval(expr),
    error = function(cnd) {
      if (max.attempts > 0) {
        Sys.sleep(sleep.seconds)
        retry_expr(expr, max.attempts - 1, sleep.seconds)
      } else {
        cnd
      }
    }
  )
}

#' Check whether file is a PNG file
#' @param path Character vector of length 1, specifing a filepath.
#' @return TRUE if file is a PNG file, FALSE otherwise.
#' @noRd
is.png <- function(path) {
  stopifnot(is.character(path))
  sig <- c(
    "89", "50", "4e", "47",
    "0d", "0a", "1a", "0a"
  )

  data <- readBin(con = path, 
                  what = "raw", 
                  n = length(sig))
  all(data == sig)
}

#' Check whether PNG file is complete.
#' @param path Character vector of length 1, specifing a filepath.
#' @return TRUE if \code{path} is a PNG file and complete, FALSE otherwise.
#' @noRd
is.complete.png <- function(path) {
  is.png(path)

  # ending chunk
  sig <- c(
    "00", "00", "00", "00",
    "49", "45", "4e", "44",
    "ae", "42", "60", "82"
  )

  data <- tail(x = readBin(con = path, 
                           what = "raw", 
                           n = file.size(path)), 
               n = length(sig))
  all(data == sig)
}

#' Check whether file is a PDF
#' @param path Character vector of length 1, specifing a filepath.
#' @return TRUE if file is a PDF file, FALSE otherwise.
#' @noRd
is.pdf <- function(path) {
  stopifnot(is.character(path))
  grepl(pattern = "^\\%PDF", 
        x = readLines(con = path, n = 1))
}

#' Check whether PDF is completely rendered
#' @param path Character vector of length 1, specifing a filepath.
#' @return TRUE if \code{path} is a PDF file and complete, FALSE otherwise.
#' @noRd
is.complete.pdf <- function(path) {
  stopifnot(is.pdf(path))
  grepl(pattern = "\\%\\%EOF$", 
        x = tail(x = readLines(con = path), 
                 n = 1L))
}

#' A wrapper for \code{knitr::include_graphics()} that includes check-and-wait.
#' @param path Character vector of length 1, specifing a filepath.
#' @param max.attempts Maximum number of times \code{expr} will be evaluated, defaults to 3.
#' @param sleep.seconds Number of seconds to wait after each attempt, defaults to 0.1.
#' @param ... additional arguments for \code{knitr::include_graphics()}.
#' @return Either the same return value of \code{knitr::include_graphics(path)} or
#' @noRd
retry_include_graphics <- function(path,
                                   max.attempts = maxima.options$max.attempts,
                                   sleep.seconds = maxima.options$sleep.seconds,
                                   ...) {
  stopifnot(
    is.character(path),
    is.integer(max.attempts),
    is.numeric(sleep.seconds)
  )

  if (is_html_output()) {
    fun <- is.complete.png
  } else {
    fun <- is.complete.pdf
  }

  retry(stopifnot(fun(path)),
    max.attempts = max.attempts,
    sleep.seconds = sleep.seconds
  )

  knitr::include_graphics(path, ...)
}

called_from_fn <- function(pattern) {
  call_st <- lapply(sys.calls(), `[[`, 1)
  any(unlist(lapply(call_st, function(x) grepl(pattern, deparse(x)))))
}

is_html_output <- function() {
  if(is.null(p <- knitr::opts_knit$get("rmarkdown.pandoc.to")))
    return(FALSE)
  knitr::is_html_output() & p == "html"
}

is_interactive <- function() {
  is.null(knitr::all_labels(engine == "maxima"))
}

#' rim_global
#' 
#' Returns knitr::knit_global() unless this returs the GlobalEnv() in which case it returns maxima.env, rim's internal environment.
#' Main purpose is to prevent `knitr::knit_global()` from returning `globalenv()`.
#'
#' @return An environment, either the result of knitr::knit_global or maxima.env
#' @export
rim_global <- function() {
  kg <- knitr::knit_global()
  if(identical(globalenv(), kg))
     maxima.env
  else
    kg
}
