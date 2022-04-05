#' Sum Quantities In Tabular Data
#'
#' All of the variables in `input_data` whose names end in `_qty` will be summed. Corresponding variables whose names end in `_unit` will be preserved. Ignores `NA`s.
#'
#' @rdname sum_quantities_by
#'
#' @param input_data (tabular)
#' @param ... variables to group by
#' @param digits (integer) passed to [total()]
#' @param signif (integer) passed to [total()]
#' @param verbose (logical)
#'
#' @importFrom units drop_units
#' @importFrom unittools restore_units
#' @importFrom tidyselect vars_select
#' @importFrom dplyr matches select group_vars across
#' @importFrom stringr str_replace_all
#' @importFrom strtools str_csv str_and
#'
#' @examples
#' df <- tibble(year = 1990:1993, foo = rep(c("bar", "baz"), 2), ems_qty = 1:4, ems_unit = "ton/yr")
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

  msg <- function (...) if(isTRUE(verbose)) message("[sum_quantities_by] ", ...)

  input_vars <-
    names(input_data)

  qty_vars <-
    tidyselect::vars_select(
      input_vars,
      dplyr::matches("_qty$"))

  msg("qty_vars is: ", str_csv(qty_vars))

  unit_vars <-
    intersect(
      input_vars,
      stringr::str_replace_all(
        qty_vars,
        "_qty$",
        "_unit"))

  by_vars <-
    union(
      unit_vars,
      tidyselect::vars_select(
        input_vars,
        ...))

  msg("by_vars is: ", str_csv(by_vars))

  grouped_data <-
    group_by(
      input_data,
      across(
        all_of(by_vars)))

  msg("summing ", strtools::str_and(qty_vars),
      " by ", strtools::str_csv(dplyr::group_vars(grouped_data)))

  summed_data <-
    summarise(
      units::drop_units(grouped_data),
      across(
        all_of(qty_vars),
        total,
        digits = digits,
        signif = signif,
        verbose = verbose),
      .groups = "drop")

  restored_data <-
    unittools::restore_units(
      summed_data,
      from = grouped_data)

  tidied_data <-
    dplyr::select(
      restored_data,
      by_vars,
      qty_vars,
      -unit_vars,
      everything(),
      unit_vars)

  # msg("adding 'inventory' class")
  # class(tidied_data) <-
  #   union(
  #     c("inventory"),
  #     class(tidied_data))

  return(tidied_data)

}

