#' Total selected columns of a data frame
#'
#' @note Typically used in conjunction with `group_by`
#'
#' @param input_data input_data data frame
#' @param \dots bare names of variables (columns) to total
#'
#' @export
total_each <- function (input_data, ..., digits = Inf, signif = Inf) {
  # WAS: summarise_each_(input_data, funs(total(., digits)), lazyeval::lazy_dots(...))
  summarise_at(
    input_data,
    vars(...),
    ~ total(., digits = digits, signif = signif))
}
