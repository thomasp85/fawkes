#' Render an SVG file with the AxiDraw penplotter
#'
#' This function takes an SVG representation of a graphic and sends it to the
#' AxiDraw penplotter for drawing. This function uses the build-in svg drawing
#' capabilities of the pyaxidraw library so rendering an svg with
#' [grDevices::svg()] and then using `axi_svg` to draw it will likely produce
#' a different result than rendering directly to [axi_dev()].
#'
#' @param file An svg file
#' @param options A list of options for the AxiDraw. Use [svg_options()] for
#' help with creating a valid list of options.
#' @param capture Logical. Should the drawing instructions be captured and
#' returned as a new svg.
#' @param text An svg as a character vector.
#'
#' @return If `capture = TRUE` a new svg showing the pen plotter movement that
#' will be used for drawing.
#'
#' @export
axi_svg <- function(file, options = list(), capture = FALSE, text) {
  if (missing(file) && !missing(text)) {
    file <- tempfile(fileext = '.svg')
    cat(text, file = file)
    invisible(on.exit(unlink(file)))
  }
  file <- fs::path_expand(file)
  axidraw <- import_axidraw()
  axidraw$plot_setup(file)
  for (opt in names(options)) {
    axidraw$options[[opt]] <- options[[opt]]
  }
  axidraw$plot_run(capture)
}
