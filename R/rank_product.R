#' @export
rank_product <- function (...) {

  data.frame(...) %>%
    mutate_all(
      ~ rank_fraction(.)) %>% # WAS: mutate_each(funs(rank_fraction)) %>%
    apply(
      MARGIN = 1,
      GM)

}
