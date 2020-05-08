#' Select top q rows (by value)
#'
#' @param x (tabular data)
#' @param q (numeric) between 0 and 1; passed to [quantile()]
#' @param wt the variable to use for ordering.
#' @param .arrange (logical) sort the results by `wt`?
#' @param na.rm (logical) ignore `NA`s in `wt`?
#'
#' @examples
#' top_q(mtcars, q = 0.9, wt = mpg)
#'
#' @importFrom rlang sym inform quo eval_tidy enquo quo_is_missing
#' @importFrom glue glue
#'
#' @export
top_q <- function (x, q, wt, na.rm = TRUE, .arrange = FALSE, verbose = getOption("verbose")) {

  wt <- rlang::enquo(wt)
  if (rlang::quo_is_missing(wt)) {
    vars <- dplyr::tbl_vars(x)
    wt_name <- dplyr::vars[length(vars)]
    rlang::inform(glue::glue("Selecting by ", wt_name))
    wt <- rlang::sym(wt_name)
  }
  if (!is.numeric(q) | (q > 1) | q < -1) {
    abort("`q` must be between -1 and 1")
  }

  if (q > 0) {
    quo <- rlang::quo(dplyr::filter(x, !!wt >= quantile(!!wt, q)))
  } else {
    quo <- rlang::quo(dplyr::filter(x, !!wt < quantile(!!wt, 1 + q)))
  }

  result <- rlang::eval_tidy(quo)

  if (isTRUE(.arrange)) {
    if (wt < 0) {
      result <- dplyr::arrange(result, dplyr::desc(!!wt))
    } else {
      result <- dplyr::arrange(result, !!wt)
    }
  }

  return(result)

}
