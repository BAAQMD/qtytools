.onLoad <- function(libname, pkgname) {

  try(
    units::install_symbolic_unit(
      name = "TCO2eq",
      warn = FALSE, # warn if already installed
      dimensionless = TRUE))

  sink_error <- function (e) {
    return(invisible(e))
  }

  sink(file = "/dev/null")

  tryCatch(
    units::install_conversion_constant(
      from = "g.CO2",
      to = "TCO2eq",
      const = 1e6),
    error = sink_error)

  tryCatch(
    units::install_conversion_constant(
      from = "TCO2eq",
      to = "MMTCO2eq",
      const = 1e-6),
    error = sink_error)

  sink(file = NULL)

}
