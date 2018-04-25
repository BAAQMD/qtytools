#' Safely coerce to integer(s)
#'
#' @param x character or factor
#' @return integer
#'
#' @importFrom readr parse_integer
#'
#' @export
#'
#' @examples
#' int(1, 3:5)
#' int(as.factor(c(foo = 2, bar = 99)))
#' int(ALA = 1, CC = 2)
#' int(1:5)
#'
int <- function (...) {

  args <- list(...)
  x <- unlist(args, recursive = TRUE, use.names = TRUE)

  if (is.factor(x)) {
    plain <- as.character(x)
    parsed <- readr::parse_integer(plain)
  } else {
    parsed <- as.integer(x)
  }

  # Restore names
  named <- setNames(parsed, names(x))
  return(named)

}
