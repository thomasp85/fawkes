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
#' - `axi_pen_test()` will test the look of filled areas with respect to a
#'   specific pen setting
#' - `axi_pen_align()` allows you to test different offset values between two
#'   pens to determine the correct setting.
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
#' @export
axi_off <- function() {
  axi_align()
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
#' @rdname axi_exec
#'
#' @param pen_a,pen_b The two pen specifications that should be tested for
#' alignment with each other
#' @importFrom grid grid.newpage grid.circle grid.polyline grid.polygon unit gpar
#' @importFrom grDevices dev.off
#' @export
axi_align_test <- function(pen_a, pen_b) {
  color_a <- pen_a[[1]]$color
  color_b <- pen_b[[1]]$color
  axi_dev(portrait = FALSE, margins = 0, color = NA, tip_size = 0, line_overlap = 0,
          ignore_color = FALSE, ignore_lwd = FALSE, pens = c(pen_a, pen_b))
  grid.newpage()
  # Pen A
  grid.circle(unit(1.5, 'cm'), unit(1, 'npc') - unit(1.5, 'cm'), unit(1, 'cm'),
              gp = gpar(fill = color_a, col = NA))
  angle <- seq(-pi/2 + pi/4, pi/2 + pi/4, length.out = 180)
  grid.polyline(unit(4 + cos(angle), 'cm'), unit(1, 'npc') - unit(1.5 + sin(angle), 'cm'),
            gp = gpar(fill = NA, col = color_a))
  grid.polygon(unit(6.5 + c(-1, -1, 1), 'cm'), unit(1, 'npc') - unit(1.5 + c(-1, 1, 1), 'cm'),
               gp = gpar(fill = color_a, col = NA))
  # Pen B
  grid.circle(unit(1.5, 'cm'), unit(1, 'npc') - unit(1.5, 'cm'), unit(1, 'cm'),
              gp = gpar(fill = NA, col = color_b))
  angle <- seq(pi/2 + pi/4, pi + pi/2 + pi/4, length.out = 180)
  grid.polyline(unit(4 + cos(angle), 'cm'), unit(1, 'npc') - unit(1.5 + sin(angle), 'cm'),
            gp = gpar(fill = NA, col = color_b))
  grid.polygon(unit(6.5 + c(-1, 1, 1), 'cm'), unit(1, 'npc') - unit(1.5 + c(-1, -1, 1), 'cm'),
               gp = gpar(fill = color_b, col = NA))
  dev.off()
}
#' @rdname axi_exec
#'
#' @param x_offset,y_offset The offsets in mm to test
#' @export
axi_pen_align <- function(x_offset = 0, y_offset = 0) {
  n <- max(length(x_offset), length(y_offset))
  x_offset <- rep_len(x_offset, n) / 10
  y_offset <- rep_len(y_offset, n) / 10
  ad <- axi_manual(units = 'cm', options = axi_options())
  ad$connect()
  cli::cli_alert_info('Insert reference pen and press enter')
  readline()
  ad$move_rel(0, 1)
  for (i in seq_along(x_offset)) {
    ad$move_rel(1, 0)
    ad$line_rel(1, 1)
    ad$move_rel(-1, 0)
    ad$line_rel(1, -1)
  }
  ad$move_to(0, 0)
  cli::cli_alert_info('Switch to offset pen and press enter')
  readline()
  ad$move_rel(0, 1.5)
  for (i in seq_along(x_offset)) {
    ad$move_rel(1.5, -0.5)
    ad$move_rel(-x_offset[i], -y_offset[i])
    ad$line_rel(0, 1)
    ad$move_rel(-0.5, -0.5)
    ad$line_rel(1, 0)
    ad$move_rel(x_offset[i], y_offset[i])
  }
  ad$move_to(0, 0)
  ad$disconnect()
}

empty_axi <- function() {
  axidraw <- import_axidraw()
  axidraw$plot_setup(system.file('empty.svg', package = 'fawkes'))
  axidraw
}
