expect_equal_data <- function (...) {
  expect_equal(..., ignore_col_order = TRUE, ignore_row_order = TRUE)
}

# Make this reproducible
n <- 2; set.seed(1)

ems_and_tput <-
  expand_grid(
    year = 1990 + seq_len(n),
    cat_id = c(77, 88),
    pol_abbr = c("PM", "TOG"),
    ems_unit = "tons/yr",
    tput_unit = "MMscf") %>%
  mutate(
    ems_qty = round(runif(n = nrow(.), min = 0, max = 100)),
    tput_qty = round(runif(n = nrow(.), min = 0, max = 100)))

ems_only <-
  dplyr::select(
    ems_and_tput,
    -dplyr::matches("tput_"))

tput_only <-
  dplyr::select(
    ems_and_tput,
    -dplyr::matches("ems_"))

tput_by_year <-
  tput_only %>%
  group_by(
    year,
    tput_unit) %>%
  summarise_at(
    vars(tput_qty),
    ~ sum(., na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(
    year,
    tput_qty,
    tput_unit)

ems_and_tput_by_year <-
  ems_and_tput %>%
  group_by(
    year,
    ems_unit,
    tput_unit) %>%
  summarise_at(
    vars(ems_qty, tput_qty),
    ~ sum(., na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(
    year,
    ems_qty,
    ems_unit,
    tput_qty,
    tput_unit)

ems_by_pol <-
  ems_and_tput %>%
  group_by(
    pol_abbr,
    ems_unit) %>%
  summarise_at(
    vars(ems_qty),
    ~ sum(., na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(
    pol_abbr,
    ems_qty,
    ems_unit)

ems_by_year_and_pol <-
  ems_and_tput %>%
  group_by(
    year,
    pol_abbr,
    ems_unit) %>%
  summarise_at(
    vars(ems_qty),
    ~ sum(., na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(
    year,
    pol_abbr,
    ems_qty,
    ems_unit)

ems_and_tput_by_year_and_pol <-
  ems_and_tput %>%
  group_by(
    year,
    pol_abbr,
    ems_unit,
    tput_unit) %>%
  summarise_at(
    vars(ems_qty, tput_qty),
    ~ sum(., na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(
    year,
    pol_abbr,
    ems_qty,
    ems_unit,
    tput_qty,
    tput_unit)
