#' Add numbers, ignoring NAs
#'
#' @param x numeric vector
#' @param digits for \code{round}
#' @param signif for \code{signif}
#'
#' @return sum of (non-missing) values in \code{x}
#'
#' @export
total <- function (x, na.rm = TRUE, digits = Inf, signif = Inf) {

  if (!is.numeric(x)) {
    stop("input must be numeric")
  }

  # as.numeric() helps prevent integer overflows
  totaled <- sum(as.numeric(x), na.rm = na.rm)

  if (is.finite(digits)) {
    totaled <- base::round(totaled, digits = digits)
  } else if (is.finite(signif)) {
    totaled <- base::signif(totaled, digits = signif)

  }

  return(totaled)

}
