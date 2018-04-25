#' Test for integer-ish-ness
#'
#' @description Is `x`` equivalent to the integer (rounded) version of `x`? Works for doubles, etc.
#'
#' @param x numeric or integer
#' @param tolerance passed to `all.equal`
#'
#' @note Probably someone else has implemented a better version
#'
#' @export
is_int <- function (x, tolerance = sqrt(.Machine$double.eps)) {

  criteria <- c(
    !is.factor(x),
    is_double(x) | is_integer(x))

  if (all_true(criteria)) {
    equality_test <- all.equal(x, round(x), tolerance = tolerance)
    return(isTRUE(equality_test))
  } else {
    return(FALSE)
  }

}
