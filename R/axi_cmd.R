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
#' @rdname axi_exec
#'
#' @param tip_size The size of the tip (i.e. the width of the line it draws) of
#' the initial pen
#' @param line_overlap The overlap between adjacent pen strokes when filling out
#' shapes and drawing thick lines, in mm. Setting this to a negative amount will
#' cause gaps between the lines.
#' @importFrom grid grid.newpage grid.circle grid.segments unit gpar
#' @importFrom grDevices dev.off
#' @export
axi_pen_test <- function(tip_size, line_overlap, options = axi_options()) {
  axi_dev(portrait = FALSE, margins = 0, tip_size = tip_size, line_overlap = line_overlap,
          options = options)
  grid.newpage()
  grid.circle(0, 1, unit(2, 'cm') * tip_size, gp = gpar(fill = 'black', col = NA))
  grid.segments(unit(1, 'cm') * tip_size, unit(1, 'npc') - unit(4, 'cm') * tip_size,
                unit(4, 'cm') * tip_size, unit(1, 'npc') - unit(1, 'cm') * tip_size,
                gp = gpar(lwd = 20 * tip_size))
  dev.off()
}

empty_axi <- function() {
  axidraw <- import_axidraw()
  axidraw$plot_setup(system.file('empty.svg', package = 'fawkes'))
  axidraw
}
