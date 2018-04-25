#' @export
maximum <- function (x) UseMethod("maximum", x)
maximum.character <- function (x) last(sort(unique(x)))
maximum.default <- function (x) max(x, na.rm = TRUE)
