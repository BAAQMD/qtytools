#' Sum Emissions In Tabular Data
#'
#' All of the variables in `input_data` whose names end in `ems_qty` will be summed. `ems_unit` will be preserved. Ignores `NA`s.
#'
#' @param input_data (tabular)
#' @param ... variables to group by
#' @param digits (integer) passed to [total()]
#' @param signif (integer) passed to [total()]
#' @param verbose (logical)
#'
#' @rdname sum_emissions
#'
#' @family sum_by
#'
#' @export
sum_emissions_by <- function (
  input_data,
  ...,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  input_vars <-
    names(input_data)

  qty_vars <-
    tidyselect::vars_select(
      input_vars,
      tidyselect::matches("_qty$"))

  #unit_vars <- str_replace_all(qty_vars, "_qty$", "_unit")

  # Drop all `_qty` vars other than `ems_qty`
  prepared_data <-
    drop_vars(
      input_data,
      setdiff(qty_vars, "ems_qty"))

  summed_data <-
    sum_quantities_by(
      prepared_data,
      ...,
      digits = digits,
      signif = signif,
      verbose = verbose)

  return(summed_data)

}
