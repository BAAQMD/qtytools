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
#' @export
top_q <- function (x, q, wt, na.rm = TRUE, .arrange = FALSE, verbose = getOption("verbose")) {

  suppressPackageStartupMessages({
    require(lazyeval)
    require(rlang)
    require(dplyr)
    require(glue)
  })

  wt <- enquo(wt)
  if (quo_is_missing(wt)) {
    vars <- tbl_vars(x)
    wt_name <- vars[length(vars)]
    inform(glue("Selecting by ", wt_name))
    wt <- sym(wt_name)
  }
  if (!is.numeric(q) | (q > 1) | q < -1) {
    abort("`q` must be between -1 and 1")
  }

  if (q > 0) {
    quo <- quo(filter(x, !!wt >= quantile(!!wt, q)))
  } else {
    quo <- quo(filter(x, !!wt < quantile(!!wt, 1 + q)))
  }

  result <- eval_tidy(quo)

  if (isTRUE(.arrange)) {
    if (wt < 0) {
      result <- arrange(result, desc(!!wt))
    } else {
      result <- arrange(result, !!wt)
    }
  }

  return(result)

}
