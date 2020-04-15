#' Render an SVG file with the AxiDraw penplotter
#'
#' This function takes an SVG representation of a graphic and sends it to the
#' AxiDraw penplotter for drawing. This function uses the build-in svg drawing
#' capabilities of the pyaxidraw library so rendering an svg with
#' [grDevices::svg()] and then using `axi_svg` to draw it will likely produce
#' a different result than rendering directly to [axi_dev()].
#'
#' @param file An svg file
#' @param capture Logical. Should the drawing instructions be captured and
#' returned as a new svg.
#' @param options An `axi_options` object. See the documentation for
#' [axi_options()] for all the settings.
#' @param layer The layer to plot when `mode = 'layers'`. See the details
#' section
#' @param copies The number of copies to produce
#' @param page_delay The delay in seconds between each copy
#' @param auto_rotate Automatically rotate the image to maximize paper usage
#' @param rendering The type of preview to generate when `preview = TRUE`.
#' Either `'none'` (nothing rendered), `'down'` (render when pen is down),
#' `'up'` (render when pen is up), or `'all'` (render everything)
#' @param reorder The type of automatical line reordering to perform. Either
#' `'none'` (no reordering), `'objects'` (reorder within objects), `'groups'` (
#' reorder within objects, then reorder groups), or `'full'` (reorder everything
#' together).
#' @param summary Should time spend and distance travel be reported in the end
#' @param text An svg as a character vector.
#'
#' @return If `capture = TRUE` a new svg showing the pen plotter movement that
#' will be used for drawing.
#'
#' @export
#'
#' @examples
#' logo <- system.file('example.svg', package = 'fawkes')
#' logo_penned <- axi_svg(logo, capture = TRUE)
#'
axi_svg <- function(file, capture = FALSE, options = axi_options(),
                    layer = NULL, copies = 1L, page_delay = 15,
                    auto_rotate = TRUE, rendering = 'all', reorder = 'none',
                    summary = FALSE, text) {
  if (missing(file) && !missing(text)) {
    file <- tempfile(fileext = '.svg')
    cat(text, file = file)
    invisible(on.exit(unlink(file)))
  }
  file <- fs::path_expand(file)
  axidraw <- import_axidraw()
  axidraw$plot_setup(file)
  axidraw$options$mode <- if (is.null(layer)) 'plot' else 'layers'
  if (!is.null(layer)) {
    axidraw$options$layer <- check_range(layer, 1, 1000)
  }
  axidraw$options$copies <- check_range(copies, 1, 9999)
  axidraw$options$page_delay <- check_range(page_delay, 0, Inf, FALSE)
  axidraw$options$auto_rotate <- check_flag(auto_rotate)
  axidraw$options$preview <- check_flag(capture)
  axidraw$options$rendering <- match(match.arg(rendering, render_ops), render_ops) - 1L
  axidraw$options$reordering <- match(match.arg(reorder, reorder_ops), reorder_ops) - 1L
  axidraw$options$report_time <- check_flag(summary)
  axidraw <- set_options(axidraw, options)
  invisible(axidraw$plot_run(capture))
}

render_ops <- c('none', 'down', 'up', 'all')
reorder_ops <- c('none', 'objects', 'groups', 'full')

