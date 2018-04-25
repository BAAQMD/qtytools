#' @describeIn total_quantities_by Totals by `year``
#'
#' @examples
#' df <- data_frame(year = 1990:1993, foo = rep(c("bar", "baz"), 2), ems_qty = 1:4, ems_unit = "tons/yr")
#' df %>% annual_quantities_by(foo, verbose = TRUE)
#'
#' @export
annual_quantities_by <- function (input_data, ..., digits = Inf, signif = Inf, verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[annual_quantities_by] ", ...)

  year_var <- vartools::find_var(input_data, suffix = "year")
  msg("year_var is: ", year_var)

  # Dispatch to `total_quantities_by()`
  totaled <- total_quantities_by(
    input_data,
    year_var,
    ...,
    digits = digits,
    signif = signif,
    verbose = verbose)

  # Always return `year` first
  select(totaled, year_var, everything())

}


#' Total quantities by year
#'
#' @export
annual_quantities_by0 <- function (input_data, ..., qty_var, unit_var, year_var = "year", digits = Inf, signif = Inf) {

  grp_vars <- select_vars(names(input_data), ...)
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
  select(totaled, dplyr::one_of(year_var), everything())

}
