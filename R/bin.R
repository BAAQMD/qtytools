#' Like `round()`, but you're not limited to 0.1s, 0.01s, etc.
#'
#' @examples
#' x <- 0:8
#' bin(x, by = 3)
#'
#' @export
bin <- function (x, by = 1) {
  return(by * (x %/% by))
}
