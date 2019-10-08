#' A graphic device that renders using the AxiDraw pen plooter
#'
#' This call opens up a graphic device that takes plotting instructions from
#' e.g. `plot()` or ggplot2.
#'
#' @param paper_size The size of the paper to draw on, either as a numeric
#' vector  giving dimensions in mm, or as a standard paper size name.
#' @param portrait Logical. Is the paper oriented as portrait
#' (i.e. width < height). Will rearrange given paper dimensions to fit.
#' @param margins. The margins of the paper, in mm. The spec follows that of css,
#' meaning that if it is given as a single value it defines all margins, if
#' given as two values it defines top/bottom, left/right, if it is given as
#' three values it defines top, left/right, bottom, and if it is given as four
#' values it defines top, right, bottom, left.
#' @param tip_size The size of the tip (i.e. the width of the line it draws) of
#' the initial pen
#' @param color The color of the initial pen
#' @param ignore_color Logical. Should changes in color output be ignored? If
#' `TRUE` the plotter will not pause for pen change when the colour changes
#' @param draw_fill Logical. Should fill be drawn using hatching?
#' @param connect_hatch Logical. Should the hatchline be drawn as connected?
#' @param hatch_gap The gap between hatchlines in mm. Set to `0` to draw as
#' continuous fill. Set to `'alpha'` to let it be determined by the alpha
#' component of the fill color.
#' @param hatch_angle The angle of the hatch lines in degrees. Set to `'random'`
#' to pick a random angle for each fill. Set to `'auto'` to pick the best angle
#' for each fill.
#' @param crosshatch Logical. Should fill be drawn with cross-hatching?
#' @param optimise_order Logical. Should the drawing order be optimised so that
#' pen movement is minimised? Will only affect consecutive calls to the same
#' drawing primitive.
#' @param options A list of options for the AxiDraw. Use [manual_options()] for
#' help with creating a valid list of options.
#'
#' @note **UNDER CONSTRUCTION**
#'
axi_dev <- function(paper_size = "A4", portrait = TRUE, margins = 0, tip_size = 1,
                    color = 'black', ignore_color = TRUE, draw_fill = TRUE,
                    connect_hatch = TRUE, hatch_gap = 0, hatch_angle = 0,
                    crosshatch = FALSE, optimise_order = TRUE,
                    options = list()) {
  size <- paper_dimensions(paper_size, portrait)
  margins <- expand_margins(margins)
  size <- size - c(margins[2] + margins[4], margins[1] + margins[3])
  if (any(size <= 0)) {
    rlang::abort("margins larger than the paper size")
  }
  axidraw <- axi_manual(options)
  devout::rdevice("axidraw_callback", ad = axidraw, size = size, flip = portrait,
    offset = margins[c(1, 4)], tip_size = tip_size, color = color,
    ignore_color = ignore_color, draw_fill = draw_fill,
    connect_hatch = connect_hatch, hatch_gap = hatch_gap,
    hatch_angle = hatch_angle, crosshatch = crosshatch,
    optimise_order = optimise_order)
}

axidraw_callback <- function(device_call, args, state) {
  state <- switch(device_call,
    open = .axi_open(args, state),
    close = .axi_close(args, state),
    circle = .axi_circle(args, state),
    rect = .axi_rect(args, state),
    line = .axi_line(args, state),
    polyline = .axi_polyline(args, state),
    polygon = .axi_polygon(args, state),
    path = .axi_path(args, state)
  )
  state
}
