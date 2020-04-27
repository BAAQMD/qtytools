#' Linear interpolation
#'
#' @param object (see Details) must contain values to be interpolated
#' @param ... further arguments passed to [stats::approx()]
#' @param verbose (logical) display messages
#'
#' @examples
#' year <- c(1991, 1993, 1995, 1998)
#' sales <- c(21, NA, 40, 45)
#' sales_model <- lerp(sales ~ year, data = data.frame(year, sales))
#'
#' @export
lerp <- function (object, ... ) {
  UseMethod("lerp")
}

#' @family lerp
#'
#' @describeIn lerp replace NAs with interpolated values
#'
#' @param x (vector) x-coords; if not supplied, values to be interpolated are assumed to be evenly spaced
#' @param xout (numeric) points at which to make predictions
#'
#' @details Names may be assigned to the resulting vector. If `nm` is `FALSE`, then no names will be assigned.
#'
#' @examples
#' lerp(sales)
#' lerp(sales, year)
#'
#' @export
lerp.default <- function (
  object,
  x = NULL,
  xout = NULL,
  nm = FALSE,
  extrapolate = FALSE,
  ...,
  verbose = getOption("verbose"))
{

  msg <- function (...) if(isTRUE(verbose)) message("[lerp.default] ", ...)

  if (!is.numeric(object)) {
    err_msg <- "[lerp] expecting numeric-ish input"
    stop(err_msg)
  } else {
    y <- object
  }

  if (!is.null(xout)) {
    if (isTRUE(nm)) {
      nm <- as.character(xout)
      msg("basing nm on xout: ", str_trunc(str_csv(nm), 20))
    }
  }

  if (is.null(x)) {
    x <- seq_along(y)
  } else {
    if (isTRUE(nm)) {
      nm <- as.character(x)
      msg("basing nm on x: ", str_trunc(str_csv(nm), 20))
    }
  }

  if (is.null(xout)) {
    msg("basing x on xout: ", str_trunc(str_csv(xout), 20))
    xout <- x
  }

  msg("x is: ", str_csv(x))
  msg("xout is: ", str_csv(xout))

  #
  # `stats::approx()` expects to see an (optional) `rule` argument.
  # We prefer a slightly more humanized `extrapolate` argument.
  #
  if (isFALSE(extrapolate)) {
    rule <- 1 # return NAs on both sides, left and right
  } else {
    if (isTRUE(extrapolate)) {
      rule <- 2  # use data at closest extreme
    } else {
      rule <- extrapolate # can mix, e.g. c(1, 2)
    }
  }

  #
  # Do the calculation, relying on stats::approx().
  #
  # Coercing `x` and `xout` with `as.numeric()` has the intended effect of
  # transforming "years" (objects of class `YYYY`, and its descendants, like
  # `CY`) into integers.
  #
  pred_obj <-
    stats::approx(
      x = as.numeric(x),        # coerce to numeric
      y = y,                    # see `stopifnot(is.numeric(y))`, above
      xout = as.numeric(xout),  # coerce to numeric
      rule = rule,
      ...)

  # Extract the predicted values.
  pred_values <- pred_obj[["y"]]

  # (Possibly) assign names to the result.
  if (!isFALSE(nm)) {
    names(pred_values) <- nm
  }

  return(pred_values)

}

#' @family lerp
#' @describeIn lerp calculate values using a formula
#' @importFrom rlang f_lhs f_rhs
#'
#' @examples
#' predict(sales_model, tibble::tibble(year = 1990:2000))
#' predict(sales_model, tibble::tibble(year = 1990:2000), extrapolate = TRUE)
#'
#' @export
predict.lerp <- function (
  object,
  envir = NULL,
  method = "linear",
  extrapolate = FALSE,
  ...,
  verbose = getOption("verbose")
) {

  model_object <- object

  if (is.null(envir)) {
    if (!is.null(model_object$data)) {
      envir <- model_object$data
    } else {
      envir <- parent.frame()
    }
  }

  # `y` is the left-hand side; `x` is the right-hand side.
  model_formula <- model_object$formula
  y <- eval(rlang::f_lhs(model_formula), envir = envir)
  x <- eval(rlang::f_rhs(model_formula), envir = envir)

  # Do the work, relying on lerp.default
  pred_values <-
    lerp.default(
      y,
      x,
      ...,
      verbose = verbose)

  return(pred_values)

}


#' @family lerp
#' @export
lerp.formula <- function (object, data = NULL, ...) {
  object <- list(formula = object, data = data)
  class(object) <- c("lerp", "list")
  values <- predict.lerp(object, ...)
  return(values)
}

#' @family lerp
#' @export
lerp.data.frame <- function (object, formula, ...) {
  object <- list(formula = formula, data = object)
  class(object) <- c("lerp", "list")
  values <- predict.lerp(object, ...)
  return(values)
}

