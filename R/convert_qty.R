#' Easily convert between different units of measurement.
#'
#' Generic function to convert a numeric vector from one scale of
#' measurement to another.
#'
#' @details
#' Relies on the nifty `units` package.
#'
#' @seealso
#' - `vignette("units", package = "units")`
#'
#' @examples
#' convert_quantities(1, "ton", "lbs") # mass
#' convert_quantities(100, "tons/day", "lbs/yr") # mass rate
#' convert_quantities(1, "yard^3", "ft^3") # volume
#' convert_quantities(212, "degF", "degC") # temperature
#'
#' @aliases converty_qty
#'
#' @export
convert_quantities <- function (x, from, to) {

  require(units)

  make_unit_ <- function (u) {
    # If at first you don't succeed, try replacing plural forms with singular forms
    tryCatch(make_unit(u), warning = function (e) {
      parts <- unlist(str_split(u, pattern = "/", n = 2))
      numerator <- parts[1]
      denominator <- parts[2]
      as_singular <- function (x) str_replace(x, "s$", "")
      recombined <- str_c(as_singular(numerator), as_singular(na.omit(denominator)), sep = "/")
      make_unit(recombined)
    })
  }

  if (is.character(from)) from <- make_unit_(from)
  if (is.character(to)) to <- make_unit_(to)

  original <- as_units(x, value = from)
  converted <- set_units(original, value = to, mode = "standard")

  return(converted)

}

#' @export
convert_qty <- convert_quantities
