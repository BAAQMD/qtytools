#' @include sum_emissions_by.R

#' @description `sum_annual_emissions_by()` is a specialized variant that always groups by `year`.
#'
#' @rdname sum_emissions
#' @usage sum_annual_emissions_by(...)
#'
#' @examples
#' df <- tibble(year = 1990:1993, foo = rep(c("bar", "baz"), 2), ems_qty = 1:4, ems_unit = "ton/yr")
#' df %>% sum_annual_emissions_by(foo, verbose = TRUE)
#'
#' @note `annual_emissions_by(...)` is equivalent to `sum_annual_emissions_by()`, but the latter is now preferred.
#'
#' @aliases annual_emissions_by
#'
#' @export
sum_annual_emissions_by <- function (
  input_data,
  ...,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  #
  # Prepend `year` to `...`
  #
  summed_emission_data <-
    sum_emissions_by(
      input_data,
      year,
      ...,
      digits = digits,
      signif = signif,
      verbose = verbose)

  return(summed_emission_data)

}

#' annual_emissions_by
#'
#' @noRd
#'
#' @export
annual_emissions_by <-
  sum_annual_emissions_by

