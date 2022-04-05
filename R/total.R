#' Add numbers, ignoring NAs
#'
#' @param x numeric vector
#' @param digits for \code{round}
#' @param signif for \code{signif}
#'
#' @importFrom stringr str_trim
#' @importFrom units set_units
#' @importFrom unittools has_units restore_units
#'
#' @return sum of (non-missing) values in \code{x}
#'
#' @export
total <- function (
  x,
  na.rm = TRUE,
  digits = Inf,
  signif = Inf,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[total] ", ...)

  if (!is.numeric(x)) {
    stop("input must be numeric")
  }

  # as.numeric() helps prevent integer overflows
  summed <- sum(as.numeric(x), na.rm = na.rm)
  if (unittools::has_units(x)) {
    summed <- unittools::restore_units(summed, from = x)
  }

  if (isTRUE(is.finite(digits)) && isTRUE(is.finite(signif))) {
    msg("WARNING: `digits` takes precedence over `signif`")
  }

  if (is.finite(digits)) {
    msg("rounding to ", digits, " digits")
    summed <- base::round(summed, digits = digits)
  } else if (is.finite(signif)) {
    msg("rounding to ", digits, " significant digits")
    summed <- base::signif(summed, digits = signif)
  }

  # try to restore class (but see integer overflow protection above)
  handle_error <- function (e) {
    msg("WARNING: ", stringr::str_trim(as.character(e)))
    msg('promoting result to class "numeric" instead of "', class(x), '"')
    return(summed)
  }

  tryCatch(
    class(summed) <- class(x),
    warning = handle_error,
    error = handle_error)

  return(summed)

}
