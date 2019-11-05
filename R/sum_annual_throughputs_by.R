#' @include sum_throughputs_by.R

#' @description `sum_annual_throughputs_by()` is a specialized variant that always groups by `year`.
#'
#' @rdname sum_throughputs
#' @usage sum_annual_throughputs_by(...)
#'
#' @examples
#' df <- tibble(year = 1990:1993, foo = rep(c("bar", "baz"), 2), tput_qty = 1:4, tput_unit = "kgal")
#' df %>% sum_annual_throughputs_by(foo, verbose = TRUE)
#'
#' @note `annual_throughputs_by(...)` is equivalent to `sum_annual_throughputs_by()`, but the latter is now preferred.
#'
#' @aliases annual_throughputs_by
#'
#' @export
sum_annual_throughputs_by <- function (
  input_data,
  ...,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  summed_data <-
    sum_throughputs_by(
      input_data,
      year,
      ...,
      digits = digits,
      signif = signif,
      verbose = verbose)

  return(summed_data)

}

#' annual_throughputs_by
#'
#' @noRd
#'
#' @export
annual_quantities_by <-
  sum_annual_quantities_by
