#' share_of
#'
#' Readable shorthand for `x / sum(x)`. Throws an error if `x` contains `NA`.
#'
#' @param x (numeric) vector with no `NA`s
#'
#' @export
#'
#' @seealso
#' - [percent_of()]
share_of <- function (x) {
  if (any(is.na(x))) stop("[share_of] x cannot contain NAs")
  shares <- x / sum(x)
  return(shares)
}
