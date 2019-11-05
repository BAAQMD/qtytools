
#' Total quantities by year
#'
#' @export
annual_quantities_by0 <- function (input_data, ..., qty_var, unit_var, year_var = "year", digits = Inf, signif = Inf) {

  .Defunct("annual_quantities_by")

  grp_vars <- tidyselect::vars_select(names(input_data), ...)
  grp_vars <- union(grp_vars, c(year = year_var))

  totaled <-
    total_quantities_by_(
      input_data,
      grp_vars = grp_vars,
      qty_var = qty_var,
      unit_var = unit_var,
      digits = digits,
      signif = signif)

  # Always return `year` first
  tidied <-
    select(
      totaled,
      dplyr::one_of(year_var),
      everything())

  class(tidied) <-
    union(
      c("annual", "inventory"),
      class(input_data))

  return(tidied)

}
