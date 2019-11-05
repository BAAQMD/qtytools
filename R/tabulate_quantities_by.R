#' Total, then spread, annual quantities
#'
#' @param input_data (tabular) must have a column ending in `_qty`
#' @param ... variables to group and spread by (see Details).
#' @param fill value for "empty cells"; see [spread()][tidyr::spread()]
#' @param digits (optional) round results to this many digits
#' @param signif (optional) round results to this many *significant* digits
#' @param verbose (logical)
#'
#' @details The last variable in `...` is used to spread (that is, to form new columns).
#'
#' @export
tabulate_quantities_by <- function (
  input_data,
  ...,
  fill = 0.0,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[tabulate_quantities_by] ", ...)

  input_vars <-
    names(input_data)

  by_vars <-
    input_vars %>%
    tidyselect::vars_select(...)

  spread_var <-
    last(by_vars)

  summed_data <-
    sum_quantities_by(
      input_data,
      !!by_vars,
      digits = digits,
      signif = signif,
      verbose = verbose)

  # `qty_var` is needed for the `spread` operation (below)
  qty_var <- find_var(input_data, suffix = "_qty")
  stopifnot(length(qty_var) == 1)

  unit_var <-
    intersect(
      names(input_data),
      str_replace(qty_var, "_qty$", "_unit"))

  stopifnot(length(unit_var) == 1)

  tabulated_data <-
    summed_data %>%
    spread(
      !!spread_var,
      !!qty_var,
      fill = fill)

  # make unit_var the last column
  msg("moving ", unit_var, " to last column")
  tidied_data <-
    tabulated_data %>%
    select(
      -unit_var,
      everything(),
      unit_var)

  msg("adding 'tabulation' class")
  class(tidied_data) <-
    union(
      c("tabulation"),
      class(input_data))

  return(tidied_data)

}
