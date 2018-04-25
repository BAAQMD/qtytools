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
tabulate_quantities_by <- function (input_data, ..., fill = 0.0, digits = Inf, signif = Inf, verbose = getOption("verbose")) {

  by_vars <- select_vars(names(input_data), ...)
  col_var <- last(by_vars)

  totaled <- total_quantities_by(
    input_data, ..., digits = digits, signif = signif, verbose = verbose)

  # `qty_var` is needed for the `spread` operation (below)
  qty_var <- find_var(input_data, suffix = "_qty")
  stopifnot(length(qty_var) == 1)

  tabulated <- spread_(
    totaled, key_col = col_var, value_col = qty_var, fill = fill)

  return(tabulated)

}
