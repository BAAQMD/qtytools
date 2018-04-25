#' @describeIn tabulate_quantities_by Tabulate `ems_qty`
#'
#' @export
tabulate_emissions_by <- function (input_data, ..., digits = Inf, signif = Inf, verbose = getOption("verbose")) {

  input_vars <- names(input_data)

  qty_vars <- select_vars(
    input_vars,
    dplyr::matches("_qty$"))

  # Drop all vars ending in `_qty` **except** `ems_qty`,
  # so that `tabulate_emissions_by()` will see only one "qty var"
  prepared <- vartools::drop_vars(
    input_data,
    setdiff(qty_vars, "ems_qty"))

  tabulated <- tabulate_quantities_by(
    prepared,
    ...,
    digits = digits,
    signif = signif,
    verbose = verbose)

  return(tabulated)

}
