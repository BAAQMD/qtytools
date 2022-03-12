#' share_of
#'
#' Readable shorthand for `x / sum(x)`. Throws an error if `x` contains `NA`.
#'
#' @param x (numeric) vector with no `NA`s
#'
#' @importFrom units set_units as_units
#'
#' @export
#'
#' @seealso
#' - [share_of()]
percent_of <- function (x) {
  shares <- share_of(x)
  percentages <- units::set_units(units::as_units(shares), "%")
  return(percentages)
}
