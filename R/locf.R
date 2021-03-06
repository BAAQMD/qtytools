#' Last Observation Carried Forward
#'
#' @param object vector or data frame
#' @param ... further arguments
#'
#' @return modified copy, with NAs replaced by last observations carried forward
#' @export
#'
#' @examples
#' sales <- c(21, NA, 40, 45)
#' locf(sales)
#' year <- c(1991, 1993, 1995, 1998)
#' mod <- locf(sales ~ year, data = data.frame(year, sales))
#' predict(mod, tibble::tibble(year = 1994:2000))
locf <- function (object, ... ) {
  UseMethod("locf")
}

#' @importFrom units as_units
#' @export
locf.units <- function (object, ...) {
  u <- units(object)
  x <- locf(units::drop_units(object), ...)
  units::as_units(x, u)
}

#' @export
locf.default <- function (object, ...) {
  i <- !is.na(object)
  x <- unname(object)
  setNames(c(x[1], x[i])[cumsum(i) + 1], names(object))
}

#' @export
locf.formula <- function (object, data, ...) {
  locf_obj <- list(formula = object, data = data)
  class(locf_obj) <- c("locf", "list")
  return(locf_obj)
}

#' @importFrom rlang set_names f_rhs f_lhs
#' @export
predict.locf <- function (object, data = NULL, ...) {

  stop()

  mod <- object

  x <- eval(rlang::f_rhs(mod$formula), envir = mod$data)
  y <- eval(rlang::f_lhs(mod$formula), envir = mod$data)
  obs <- rlang::set_names(y, as.character(x))

  if (is.null(data)) {
    xout <- x
  } else {
    xout <- eval(rlang::f_rhs(mod$formula), envir = data)
  }

  domain <- as.character(sort(union(x, xout)))
  range <- as.character(xout)

  obs %>% .[domain] %>% rlang::set_names(domain) %>% locf %>% .[range]

}
