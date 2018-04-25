#' Total emissions by year
#'
#' @description Total `ems_qty` by `year`, ..., `ems_unit`
#'
#' @examples
#' df <- data_frame(year = 1990:1993, foo = rep(c("bar", "baz"), 2), ems_qty = 1:4, ems_unit = "tons/yr")
#' df %>% annual_emissions_by(foo, verbose = TRUE)
#'
#' @seealso [annual_throughputs_by()]
#' @seealso [tabulate_emissions_by()]
#' @seealso [total_emissions_by()]
#'
#' @export
annual_emissions_by <- function (input_data, ..., digits = Inf, signif = Inf, verbose = getOption("verbose")) {

  total_emissions_by(
    input_data,
    year,
    ...,
    digits = digits,
    signif = signif,
    verbose = verbose)

  # annual_quantities_by(
  #   input_data,
  #   ...,
  #   qty_var = "ems_qty",
  #   unit_var = "ems_unit",
  #   digits = digits,
  #   signif = signif)

}
