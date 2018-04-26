#' Total throughputs by year
#'
#' @description Total `tput_qty` by `year`, ..., `tput_unit`
#'
#' @seealso [annual_emissions_by()]
#' @seealso [tabulate_throughputs_by()]
#' @seealso [total_throughputs_by()]
#'
#' @export
annual_throughputs_by <- function (input_data, ..., digits = Inf, signif = Inf, verbose = getOption("verbose")) {

  total_throughputs_by(
    input_data,
    year,
    ...,
    digits = digits,
    signif = signif,
    verbose = verbose)

}
