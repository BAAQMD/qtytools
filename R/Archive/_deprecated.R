#' @note Deprecated
#'
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
  pruned <- dplyr::select(input_data, -dplyr::matches(unit_var))

  # Group, aggregate, and ungroup
  grouped <- group_by_at(pruned, grp_vars) # TODO: add `...`
  totaled <- summarise_at(grouped, qty_var, funs(agg)) # TODO: add `na.rm`
  ungrouped <- ungroup(totaled)

  # Restore the units column
  ungrouped[[unit_var]] <- unit_value

  class(ungrouped) <-
    class(input_data)

  return(ungrouped)

}
