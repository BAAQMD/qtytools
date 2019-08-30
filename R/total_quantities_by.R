#' Sum up quantities in tabular data
#'
#' Sum, ignoring `NA`s, all of the variables in `input_data` whose names end in `_qty`.
#'
#' @param input_data (tabular)
#' @param ... variables to group by
#' @param digits (integer) passed to [total()]
#' @param signif (integer) passed to [total()]
#' @param verbose (logical)
#'
#' @examples
#' df <- data_frame(year = 1990:1993, foo = rep(c("bar", "baz"), 2), ems_qty = 1:4, ems_unit = "tons/yr")
#' df %>% total_quantities_by(foo, verbose = TRUE)
#' df %>% total_quantities_by("year", foo, verbose = TRUE)
#'
#' @seealso [total()]
#'
#' @export
total_quantities_by <- function (
  input_data,
  ...,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  require(lazyeval)
  require(rlang)

  msg <- function (...) if(isTRUE(verbose)) message("[total_quantities_by] ", ...)

  input_vars <- names(input_data)
  qty_vars <- select_vars(input_vars, dplyr::matches("_qty$"))

  unit_vars <- intersect(
    input_vars,
    str_replace_all(qty_vars, "_qty$", "_unit"))

  by_vars <- select_vars(input_vars, ...)
  msg("by_vars is: ", str_csv(by_vars))

  grouped <- group_by_(input_data, .dots = union(by_vars, unit_vars))
  msg("summing ", str_csv(qty_vars), " by ", str_csv(group_vars(grouped)))

  totaled <- summarise_at(grouped, vars(one_of(qty_vars)), funs(total))
  ungrouped <- ungroup(totaled)

  tidied <-
    select(
      ungrouped,
      by_vars,
      qty_vars,
      -unit_vars,
      everything(),
      unit_vars)

  msg("adding 'inventory' class")
  class(tidied) <-
    union(
      c("inventory"),
      class(tidied))

  return(tidied)

}

