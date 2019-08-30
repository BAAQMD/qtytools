#' @method print tabulation
#' @export
print.tabulation <- function (
  x,
  totals = NULL,
  ...
) {

  if (!is.null(totals)) {
    require(janitor)
    x <- janitor::adorn_totals(x, totals)
  }

  NextMethod("print")

}
