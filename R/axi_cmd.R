#' Execute direct commands to the AxiDraw plotter
#'
#' These small utility functions are not concerned with plotting, but rather
#' lets you query information about the plotter or execute small commands useful
#' for setup.
#'
#' @param options An `axi_options` object. See the documentation for
#' [axi_options()] for all the settings. Many are not relevant for these
#' function.
#'
#' @details
#' The different functions have various, unrelated, uses:
#' - `axi_version()` returns the version of the AxiDraw software
#' - `axi_sysinfo()` returns the EBB firmware version and system information
#' - `axi_toggle_pen()` toggles the position of the pen (raises it if lowered
#'   and vice versa)
#' - `axi_align()` raises the pen and turns off the stepper motors so the
#'   carriage can be moved freely.
#'
#' @rdname axi_exec
#' @name axi_exec
#'
NULL
#' @rdname axi_exec
#' @export
axi_version <- function(options = axi_options()) {
  ad <- empty_axi()
  ad$options$mode <- 'version'
  ad <- set_options(ad, options)
  ad$plot_run(FALSE)
}
#' @rdname axi_exec
#' @export
axi_sysinfo <- function(options = axi_options()) {
  ad <- empty_axi()
  ad$options$mode <- 'sysinfo'
  ad <- set_options(ad, options)
  ad$plot_run(FALSE)
}
#' @rdname axi_exec
#' @export
axi_toggle_pen <- function(options = axi_options()) {
  ad <- empty_axi()
  ad$options$mode <- 'toggle'
  ad <- set_options(ad, options)
  ad$plot_run(FALSE)
}
#' @rdname axi_exec
#' @export
axi_align <- function(options = axi_options()) {
  ad <- empty_axi()
  ad$options$mode <- 'align'
  ad <- set_options(ad, options)
  ad$plot_run(FALSE)
}

empty_axi <- function() {
  axidraw <- import_axidraw()
  axidraw$plot_setup(system.file('empty.svg', package = 'fawkes'))
  axidraw
}
