#' @include sum_quantities_by.R

#' @description `sum_annual_quantities_by()` is a specialized variant that always groups by `year`.
#'
#' @rdname sum_quantities_by
#' @usage sum_annual_quantities_by(...)
#'
#' @examples
#' df <- tibble(year = 1990:1993, foo = rep(c("bar", "baz"), 2), ems_qty = 1:4, ems_unit = "ton/yr")
#' df %>% sum_annual_quantities_by(foo, verbose = TRUE)
#'
#' @note `annual_quantities_by(...)` is equivalent to `sum_annual_quantities_by()`, but the latter is now preferred.
#'
#' @aliases annual_quantities_by
#'
#' @export
sum_annual_quantities_by <- function (
  input_data,
  ...,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[sum_annual_quantities_by] ", ...)

  year_var <- vartools::find_year_var(input_data, verbose = verbose)
  msg("year_var is: ", year_var)

  #
  # Prepend `year` to `...`
  #
  summed_data <-
    sum_quantities_by(
      input_data,
      year_var,
      ...,
      digits = digits,
      signif = signif,
      verbose = verbose)

  # Always return `year` first
  tidied <-
    dplyr::select(
      summed_data,
      year_var,
      everything())

  # class(tidied) <-
  #   union(
  #     c("annual"),
  #     class(input_data))

  return(tidied)

}

#' annual_quantities_by
#'
#' @noRd
#'
#' @export
annual_quantities_by <-
  sum_annual_quantities_by

