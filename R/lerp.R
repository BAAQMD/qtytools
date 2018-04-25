#' Linear Interpolation
#'
#' @param object vector or data frame
#' @param ... further arguments
#'
#' @return modified copy, with NAs replaced by linearly interpolated values
#' @export
#'
#' @examples
#' year <- c(1991, 1993, 1995, 1998)
#' sales <- c(21, NA, 40, 45)
#' lerp(year, sales)
#' sales_model <- lerp(sales ~ year, data = data.frame(year, sales))
#' predict(sales_model, data_frame(year = 1990:2000))
#' predict(sales_model, data_frame(year = 1990:2000), extrapolate = TRUE)
lerp <- function (x, ... ) {
  UseMethod("lerp")
}

#' @export
lerp.numeric <- function (x, y, xout = x, ...) {
  pred <- approx(x, y, xout, ...)$y
  names(pred) <- as.character(xout)
  return(pred)
}

#' @export
lerp.formula <- function (object, data, ...) {
  lerp_obj <- list(formula = object, data = data)
  class(lerp_obj) <- c("lerp", "list")
  return(lerp_obj)
}

#' @export
lerp.data.frame <- function (object, formula, ...) {
  lerp_obj <- list(formula = formula, data = object)
  class(lerp_obj) <- c("lerp", "list")
  return(lerp_obj)
}

#' @importFrom formula.tools lhs rhs
#' @export
predict.lerp <- function (object, data = NULL, method = "linear", extrapolate = FALSE, ...) {

  mod <- object
  require(formula.tools)

  x <- eval(rhs(mod$formula), envir = mod$data)
  y <- eval(lhs(mod$formula), envir = mod$data)
  obs <- setNames(y, as.character(x))

  if (is.null(data)) {
    xout <- x
  } else {
    xout <- eval(rhs(mod$formula), envir = data)
  }

  if (isTRUE(extrapolate)) rule = 2 else rule = 1
  pred <- lerp(x, y, xout, method = method, rule = rule, ...)
  return(pred)

}
