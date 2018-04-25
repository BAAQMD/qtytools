#' Geometric mean
#'
#' @examples
#' x <- c(4, 1, 1/32)
#' GM(x)
#'
#' @export
GM <- function (x, na.rm = TRUE) {
  if (is.null(x)) return(NULL)
  if (isTRUE(na.rm)) x <- na.omit(x)
  logged <- log(x)
  if (any(!is.finite(logged))) return(NaN)
  exp(mean(logged))
}

