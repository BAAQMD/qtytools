test_category_totals <- tibble(
  year = "CY2000",
  cat_id = int(11111, 999),
  ems_qty = c(456.7, 20.0),
  ems_unit = "tons/yr")

test_population_fractions <-
  tibble(year = "CY2000", ALA = 0.5, CC = 0.3, SON = 0.2)

test_population_growth <- bind_rows(
  ALA = tibble(CY2000 = 1.00, CY2005 = 0.97, CY2010 = 0.90),
  CC  = tibble(CY2000 = 1.00, CY2005 = 1.29, CY2010 = 1.38),
  SON = tibble(CY2000 = 1.00, CY2005 = 1.02, CY2010 = 1.03),
  .id = "cnty_abbr")

