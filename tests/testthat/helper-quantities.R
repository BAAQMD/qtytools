expect_equal_data <- function (...) {
  expect_equal(..., ignore_col_order = TRUE, ignore_row_order = TRUE)
}

# Make this reproducible
n <- 2; set.seed(1)

ems_and_tput <-
  expand_grid(year = 1990 + seq_len(n),
              cat_id = c(77, 88),
              pol_abbr = c("PM", "TOG"),
              ems_unit = "tons/yr",
              tput_unit = "MMscf") %>%
  mutate(ems_qty = round(runif(n = nrow(.), min = 0, max = 100)),
         tput_qty = round(runif(n = nrow(.), min = 0, max = 100)))

ems_only <- select(ems_and_tput, -dplyr::matches("tput_"))
tput_only <- select(ems_and_tput, -dplyr::matches("ems_"))

tput_by_year <- tibble(
  year = c(1991, 1992),
  tput_qty = c(230, 112),
  tput_unit = "MMscf")

ems_and_tput_by_year <- tibble(
  year = c(1991, 1992),
  ems_qty = c(198, 284),
  ems_unit = "tons/yr",
  tput_qty = c(230, 112),
  tput_unit = "MMscf")

ems_by_pol <- tibble(
  pol_abbr = c("PM", "TOG"),
  ems_qty = c(84 + 128, 114 + 156),
  ems_unit = "tons/yr")

ems_by_year_and_pol <- tibble(
  year = c(1991, 1991, 1992, 1992),
  pol_abbr = c("PM", "TOG", "PM", "TOG"),
  ems_qty = c(84, 114, 128, 156),
  ems_unit = "tons/yr")

ems_and_tput_by_year_and_pol <- tibble(
  year = c(1991, 1991, 1992, 1992),
  pol_abbr = c("PM", "TOG", "PM", "TOG"),
  ems_qty = c(84, 114, 128, 156),
  tput_qty = c(84, 146, 24, 88),
  ems_unit = "tons/yr",
  tput_unit = "MMscf")

BayArea_GHGRP_emission_data <-
  GHGRP::GHGRP_emission_data %>%
  semi_join(GHGRP::GHGRP_crosswalk %>% filter(!is.na(DB_fac_id)), by = "ARB_fac_id") %>%
  rename(ems_qty = MMTCO2eq) %>%
  mutate(ems_unit = "MMTCO2eq")
