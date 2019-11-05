#' @describeIn tabulate_quantities_by Tabulate `tput_qty`
#'
#' @export
tabulate_throughputs_by <- function (input_data, ..., digits = Inf, signif = Inf, verbose = getOption("verbose")) {

  input_vars <- names(input_data)

  qty_vars <-
    tidyselect::vars_select(
      input_vars,
      tidyselect::matches("_qty$"))

  # Drop all vars ending in `_qty` **except** `tput_qty`,
  # so that `tabulate_emissions_by()` will see only one "qty var"
  prepared <- vartools::drop_vars(
    input_data,
    setdiff(qty_vars, "tput_qty"))

  tabulated <- tabulate_quantities_by(
    prepared,
    ...,
    digits = digits,
    signif = signif,
    verbose = verbose)

  return(tabulated)

}
