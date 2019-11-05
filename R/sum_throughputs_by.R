#' Sum Throughputs In Tabular Data
#'
#' All of the variables in `input_data` whose names end in `tput_qty` will be summed. `tput_unit` will be preserved. Ignores `NA`s.
#'
#' @param input_data (tabular)
#' @param ... variables to group by
#' @param digits (integer) passed to [total()]
#' @param signif (integer) passed to [total()]
#' @param verbose (logical)
#'
#' @rdname sum_throughputs
#'
#' @family sum_by
#'
#' @export
sum_throughputs_by <- function (
  input_data,
  ...,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  input_vars <-
    names(input_data)

  qty_vars <-
    select_vars(
      input_vars,
      dplyr::matches("_qty$"))

  #unit_vars <- str_replace_all(qty_vars, "_qty$", "_unit")

  # Drop all `_qty` vars other than `tput_qty`
  prepared_data <-
    vartools::drop_vars(
      input_data,
      setdiff(qty_vars, "tput_qty"))

  summed_data <-
    sum_quantities_by(
      prepared_data,
      ...,
      digits = digits,
      signif = signif,
      verbose = verbose)

  return(summed_data)

}
