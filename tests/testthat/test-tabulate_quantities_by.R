context("tabulate_quantities_by")

test_that("tabulate_quantities_by (no grouping)", {

  expect_equal_data(
    tput_only %>% tabulate_quantities_by(year),
    data_frame(tput_unit = "MMscf", `1991` = 230, `1992` = 112))

  expect_equal_data(
    ems_only %>% tabulate_quantities_by(pol_abbr),
    data_frame(ems_unit = "tons/yr", PM = 212, TOG = 270))

})
