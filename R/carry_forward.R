#' Carry non-NA values forward, replacing NAs
#'
#' @note Similar to `na.locf` in package `zoo`
#'
#' @param x a vector
#'
#' @examples
#'   carry_forward(c(1, NA, 2, 3, NA, 5, 6, NA))
#'   carry_forward(c(NA, NA, NA, 1, NA, 2, 3, NA, 5, 6, NA))
#'
#' @export
carry_forward <- function (x) {

  .Deprecated("locf")

  i <- !is.na(x)
  c(x[1], x[i])[cumsum(i) + 1]

}
