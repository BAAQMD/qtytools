#' Replace out-of-range values
#'
#' @examples
#' x <- rnorm(n = 10, mean = 0, sd = 1.0)
#' trim_values(x, below = -0.1, above = 0.1, replace_with = NA)
#'
#' @export
trim_values <- function (x, below = -Inf, above = Inf, replace_with = NA) {
  i <- (x < below) | (x > above)
  replace_which(x, i, replace_with)
}
