#' Multiply numbers by a given amount
#'
#' @param input_data tbl containing columns to total
#' @param \dots additional parameters passed on to methods
#' @export
multiply_each <- function (input_data, ..., by = 1.0) {
  f <- function (x) x * by
  mutate_at(input_data, vars(...), funs(f))
}

