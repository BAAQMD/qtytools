#' @export
rank_fraction <- function (x, ties.method = "min", ...) {
  rank(-x, ties.method = ties.method, ...) / length(x)
}
