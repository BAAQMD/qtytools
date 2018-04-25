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
  unit_vars <- str_replace_all(qty_vars, "_qty$", "_unit")

  # Drop all `_qty` vars other than `tput_qty`, and all `_unit` vars other than `tput_unit`
  prepared <- drop_vars(input_data, setdiff(qty_vars, "tput_qty"), setdiff(unit_vars, "tput_unit"))

  total_quantities_by(prepared, ..., digits = digits, signif = signif, verbose = verbose)

  # total_quantities_by(
  #   input_data,
  #   ...,
  #   qty_var = "tput_qty",
  #   unit_var = "tput_unit",
  #   digits = digits,
  #   signif = signif)

}
