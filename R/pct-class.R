#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name qtytools-vctrs
NULL

# for compatibility with the S4 system
methods::setOldClass(c("qtytools_pct", "vctrs_vctr"))

#' `percent` vector
#'
#' This creates a double vector that represents percentages so when it is
#' printed, it is multiplied by 100 and suffixed with `%`.
#'
#' @param x
#'  * For `percent()`: A numeric vector
#'  * For `is_percent()`: An object to test.
#' @return An S3 vector of class `qtytools_pct`.
#' @export
#' @examples
#' percent(c(0.25, 0.5, 0.75))
percent <- function(x = double()) {
  x <- vec_cast(x, double())
  new_percent(x)
}

#' @export
#' @rdname percent
as_percent <- function(x) {
  vec_cast(x, new_percent())
}

#' @export
#' @rdname percent
is_percent <- function(x) {
  inherits(x, "qtytools_pct")
}

#' @noRd
new_percent <- function(x = double()) {
  vec_assert(x, double())
  new_vctr(x, class = "qtytools_pct")
}

#' @export
format.qtytools_pct <- function(x, ...) {
  out <- formatC(signif(vec_data(x) * 100, 3))
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

#' @export
vec_ptype_abbr.qtytools_pct <- function(x, ...) {
  "pct"
}

#'
#' @method vec_ptype2 qtytools_pct
#' @export
#' @export vec_ptype2.qtytools_pct
#' @rdname qtytools-vctrs
#'
#' @note Our percent class is coercible back and forth with double vectors.
#'
#' The default method provides a user friendly error message if the coercion
#' doesn’t exist and makes sure `NA` is handled in a standard way. `NA` is
#' technically a logical vector, but we want to stand in for a missing value of
#' any type.
#'
vec_ptype2.qtytools_pct <- function(x, y, ...) {
  UseMethod("vec_ptype2.qtytools_pct")
}

#' @method vec_ptype2.qtytools_pct qtytools_pct
#' @export
vec_ptype2.qtytools_pct.qtytools_pct <- function(x, y, ...) {
  new_percent()
}

#' @method vec_ptype2.qtytools_pct double
#' @export
vec_ptype2.qtytools_pct.double <- function(x, y, ...) {
  double()
}

#' @method vec_ptype2.double qtytools_pct
#' @export
vec_ptype2.double.qtytools_pct <- function(x, y, ...) {
  double()
}

#' @method vec_cast qtytools_pct
#' @export vec_cast.qtytools_pct
#' @export
#' @rdname qtytools-vctrs
vec_cast.qtytools_pct <- function(x, to, ...) {
  UseMethod("vec_cast.qtytools_pct")
}

#' @method vec_cast.qtytools_pct qtytools_pct
#' @export
vec_cast.qtytools_pct.qtytools_pct <- function(x, to, ...) x

#' @method vec_cast.qtytools_pct double
#' @export
vec_cast.qtytools_pct.double <- function(x, to, ...) percent(x)

#' @method vec_cast.double qtytools_pct
#' @export
vec_cast.double.qtytools_pct <- function(x, to, ...) vec_data(x)

format_qtytools_pct <- function (x, digits) {
  format <- paste0("%0.", digits, "f%%")
  sprintf(format, x * 100, justify = "right")
}

vec_arith.qtytools_pct <- function(op, x, y, ...) {
  UseMethod("vec_arith.qtytools_pct", y)
}

vec_arith.qtytools_pct.default <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = ,
    "+" = ,
    "-" = new_percent(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}

vec_arith.qtytools_pct.qtytools_pct <- vec_arith.qtytools_pct.default
vec_arith.qtytools_pct.numeric <- vec_arith.qtytools_pct.default
vec_arith.numeric.qtytools_pct <- vec_arith.qtytools_pct.default

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.qtytools_pct <- function(x, ...) {
  out <- format(x, formatter = format_qtytools_pct)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

