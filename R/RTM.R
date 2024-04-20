#' Turn R data to Maxima Format
#'
#' @param x A object.
#' @param maximaName x object name.
#' @return A string.
#' @examples
#' RTM(matrix(1), "henlo")
#' @export
RTM <- function(
    x,
    maximaName = NULL
) {
  UseMethod("RTM")
}

#' @export
RTM.default <- function(x, maximaName = NULL) stop("UNRECOGNIZED INPUT")

#' @export
RTM.matrix <- function(
  x,
  maximaName = NULL
) {
  if (!is.null(maximaName)) {
    maximaName <- paste0(maximaName, ":")
  }
  return(
    paste0(maximaName, "matrix(", paste0(
    apply(x, 1, function(y) paste0("[", paste0(y, collapse = ", "), "]")),
    collapse = ", "), ");")
  )
}
