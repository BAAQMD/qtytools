#' round_half_up
#'
#' Round according to human-friendly "round-half-up" convention.
#'
#' @details
#' R's `round()` follows the IEEE "round-to-even" rule, meaning that 1.5 will be rounded to 2, but 2.5 will also be rounded to 2. This isn't what many (most?) humans expect.
round_half_up <- function (
  x,
  digits = 0
) {
  z <- abs(x) * 10^digits + 0.5
  rounded <- trunc(z) / 10^digits
  return(rounded * sign(x))
}
