#' Multiply a column in one dataset by one or more variables from another dataset
#'
#' @importFrom tbltools fun_join
#'
#' @examples
#' demo("apply_scalars", package = "inventory")
#'
#' @export
apply_scalars <- function (input_data, using, value_col, key_col = NULL, join_na = TRUE, keep_as = NULL, verbose = FALSE, warn = TRUE) {

  # TODO: support numeric vectors too
  stopifnot(is.data.frame(using))

  if (nrow(input_data) == 0) {
    if (isTRUE(warn)) warning("`input_data` empty: returning unmodified")
    return(input_data)
  }

  join_cols <- intersect(names(input_data), names(using))
  scalar_cols <- setdiff(names(using), join_cols)

  if (verbose) {
    message("Scaling `", value_col, "` by: ", paste_csv(scalar_cols))
  }

  stopifnot(all_true(using[, scalar_cols] %>% vapply(is.numeric, FUN.VALUE = logical(1))))

  if (length(join_cols) == 0) {

    combined <- bind_cols(input_data, using)

  } else {

    by_row <- function (X, FUN, ...) apply(X, 1, FUN, ...)
    any_na <- function (x) any(is.na(x))
    i <- using[, join_cols] %>% by_row(any_na)

    if (verbose) {
      message("Joining by: ", paste_csv(join_cols))
      if (any(i)) message("Custom merging on rows: ", paste_csv(which(i)))
    }

    # Left join, treating NAs as wild
    merge_fun <- function (e1, e2) is.na(e2) | (e1 == e2)
    merged <- tbltools::fun_join(input_data, using[i, ], by = join_cols, fun = merge_fun)

    joined <- inner_join(input_data, using[!i, ], by = join_cols)
    combined <- bind_rows(merged, joined)

  }

  # Distribute the scalar(s) into the values contained in the columns named by `value_cols`
  f <- function (x) x * combined[[value_col]]
  result <- mutate_at(combined, vars(one_of(scalar_cols)), ~ f(.))

  if (is.null(keep_as)) {
    result <- dplyr::select(result, -dplyr::matches(value_col))
  }

  if (!is.null(key_col)) {

    if (!is.null(keep_as)) {

      # Save the scalar(s) themselves in a new column (named via `keep_as`)
      mutate_clause <- lazyeval::interp("new / old", new = as.name(value_col), old = as.name(".old"))
      result <- result %>%
        rename_(.dots = set_names(value_col, ".old")) %>%
        gather_(key_col, value_col, scalar_cols) %>%
        mutate_(.dots = set_names(mutate_clause, keep_as)) %>%
        dplyr::select(-dplyr::matches(".old"))

    } else {
      result <- gather_(result, key_col, value_col, scalar_cols)
    }

    # If warranted (e.g. they are years), cast gathered column names to integers
    x <- as.character(result[[key_col]])
    if (all_true(str_detect(x, pattern = "^[[:digit:]]+$"))) {
      x <- as.integer(x)
    }
    result[[key_col]] <- x
  }

  return(result)

}
