#' @importFrom janitor adorn_totals
#' @method print tabulation
#' @export
print.tabulation <- function (
  x,
  totals = NULL,
  ...
) {

  if (!is.null(totals)) {
    x <- janitor::adorn_totals(x, totals)
  }

  NextMethod("print")

}
