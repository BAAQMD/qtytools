#' @export
scale_throughputs_by <- function (input_data, using, ...) {
  apply_scalars(input_data, using, value_col = "tput_qty", ...)
}

