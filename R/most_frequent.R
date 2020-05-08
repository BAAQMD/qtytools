#' @export
most_frequent <- function(x, na.rm = TRUE) {
  if (isTRUE(na.rm)) x <- stats::na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
