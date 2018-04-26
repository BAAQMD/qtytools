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
total_quantities_by <- function (input_data, ..., digits = Inf, signif = Inf, verbose = getOption("verbose")) {

  require(lazyeval)
  require(rlang)

  msg <- function (...) if(isTRUE(verbose)) message("[total_quantities_by] ", ...)

  input_vars <- names(input_data)
  qty_vars <- select_vars(input_vars, dplyr::matches("_qty$"))
  unit_vars <- str_replace_all(qty_vars, "_qty$", "_unit")

  by_vars <- select_vars(input_vars, ...)
  msg("by_vars is: ", str_csv(by_vars))

  grouped <- group_by_(input_data, .dots = union(by_vars, unit_vars))
  msg("summing ", str_csv(qty_vars), " by ", str_csv(group_vars(grouped)))

  totaled <- summarise_at(grouped, vars(one_of(qty_vars)), funs(total))
  ungrouped <- ungroup(totaled)

  select(ungrouped, by_vars, qty_vars, unit_vars, everything())

}

total_quantities_by_ <- function (input_data, grp_vars, qty_var = qty_var, unit_var = unit_var, digits = Inf, signif = Inf) {

  .Deprecated("total_quantities_by")

  if (missing(qty_var)) qty_var <- find_var(input_data, suffix = "_qty")
  if (missing(unit_var)) unit_var <- find_var(input_data, suffix = "_unit")

  unit_value <- unique(input_data[[unit_var]])
  is_atomic <- function (x) (length(x) == 1)
  if (!is_atomic(unit_value)) stop("Mixed units not yet supported")

  # Curry the `total` function
  agg <- partial(total, digits = digits, signif = signif)

  # Drop the units column (temporarily)
  pruned <- select(input_data, -dplyr::matches(unit_var))

  # Group, aggregate, and ungroup
  grouped <- group_by_at(pruned, grp_vars) # TODO: add `...`
  totaled <- summarise_at(grouped, qty_var, funs(agg)) # TODO: add `na.rm`
  ungrouped <- ungroup(totaled)

  # Restore the units column
  ungrouped[[unit_var]] <- unit_value
  return(ungrouped)

}
