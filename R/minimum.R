#' @export
minimum <- function (x) UseMethod("minimum", x)
minimum.character <- function (x) first(sort(unique(x)))
minimum.default <- function (x) min(x, na.rm = TRUE)
