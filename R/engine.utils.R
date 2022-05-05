#' Cluster a code chunk into commands
#' @param code Character vector containing the code chunk, one line per element. 
#' @return A \code{list} where each element holds the indices of \code{code} that make up one command, i.e. terminates with either ';' or '$' 
#' @noRd
gather <- function(code) {
  hits <- grepl(pattern = ";|\\$", x = code)
  marks <- rev(cumsum(rev(hits)))
  marks[code == ""] <- NA
  sapply(X = unique(marks), 
	 FUN = function(um, m) which(um == m), 
	 m = marks, 
	 simplify = FALSE)
}

