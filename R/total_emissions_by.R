#' Total emissions, ignoring NAs
#'
#' @description Total `ems_qty` by ...
#' @note Ignores `year` and `pol_xxx`, unless you say otherwise!
#'
#' @seealso [tabulate_throughputs_by()]
#' @seealso [annual_emissions_by()]
#'
#' @export
total_emissions_by <- function (
  input_data,
  ...,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  input_vars <- names(input_data)
  qty_vars <- select_vars(input_vars, dplyr::matches("_qty$"))

  #unit_vars <- str_replace_all(qty_vars, "_qty$", "_unit")

  # Drop all `_qty` vars other than `ems_qty`
  prepared <- drop_vars(
    input_data,
    setdiff(qty_vars, "ems_qty"))

  totaled <-
    total_quantities_by(
      prepared,
      ...,
      digits = digits,
      signif = signif,
      verbose = verbose)

  return(totaled)

}
