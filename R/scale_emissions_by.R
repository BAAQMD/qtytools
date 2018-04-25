#' @export
scale_emissions_by <- function (input_data, using, ...) {
  apply_scalars(input_data, using, value_col = "ems_qty", ...)
}
