#' Total throughputs, ignoring NAs
#'
#' @description Total `tput_qty` by ...
#' @note Ignores `year` unless you say otherwise!
#'
#' @seealso [tabulate_throughputs_by()
#' @seealso [annual_throughputs_by()]
#'
#' @export
total_throughputs_by <- function (input_data, ..., digits = Inf, signif = Inf, verbose = getOption("verbose")) {

  input_vars <- names(input_data)
  qty_vars <- select_vars(input_vars, dplyr::matches("_qty$"))

  #unit_vars <- str_replace_all(qty_vars, "_qty$", "_unit")

  # Drop all `_qty` vars other than `tput_qty`
  prepared <- drop_vars(input_data, setdiff(qty_vars, "tput_qty"))

  total_quantities_by(prepared, ..., digits = digits, signif = signif, verbose = verbose)

}
