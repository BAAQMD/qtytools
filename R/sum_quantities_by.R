#' Sum Quantities In Tabular Data
#'
#' All of the variables in `input_data` whose names end in `_qty` will be summed. Corresponding variables whose names end in `_unit` will be preserved. Ignores `NA`s.
#'
#' @param input_data (tabular)
#' @param ... variables to group by
#' @param digits (integer) passed to [total()]
#' @param signif (integer) passed to [total()]
#' @param verbose (logical)
#'
#' @examples
#' df <- tibble(year = 1990:1993, foo = rep(c("bar", "baz"), 2), ems_qty = 1:4, ems_unit = "tons/yr")
#' df %>% sum_quantities_by(foo, verbose = TRUE)
#' df %>% sum_quantities_by("year", foo, verbose = TRUE)
#' df %>% sum_quantities_by("year", foo, digits = 2, verbose = TRUE)
#'
#' @family sum_by
#'
#' @export
sum_quantities_by <- function (
  input_data,
  ...,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  require(lazyeval)
  require(rlang)

  msg <- function (...) if(isTRUE(verbose)) message("[sum_quantities_by] ", ...)

  input_vars <-
    names(input_data)

  qty_vars <-
    tidyselect::vars_select(
      input_vars, dplyr::matches("_qty$"))

  unit_vars <-
    intersect(
      input_vars,
      str_replace_all(qty_vars, "_qty$", "_unit"))

  by_vars <-
    union(
      unit_vars,
      tidyselect::vars_select(
        input_vars,
        ...))

  msg("by_vars is: ", str_csv(by_vars))

  grouped_data <-
    group_by_at(
      input_data,
      by_vars)

  msg("summing ", str_csv(qty_vars),
      " by ", str_csv(group_vars(grouped_data)))

  summed_data <-
    summarise_at(
      grouped_data,
      vars(qty_vars),
      ~ total(., digits = digits, signif = signif, verbose = verbose))

  ungrouped_data <-
    ungroup(summed_data)

  tidied_data <-
    select(
      ungrouped_data,
      by_vars,
      qty_vars,
      -unit_vars,
      everything(),
      unit_vars)

  msg("adding 'inventory' class")
  class(tidied_data) <-
    union(
      c("inventory"),
      class(tidied_data))

  return(tidied_data)

}

