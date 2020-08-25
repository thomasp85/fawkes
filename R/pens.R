#' Describe the characteristics of a pen
#'
#' Pen plots can include ink from many pens and how they behave may differ.
#' While some of these can be set when a new color is encountered it is often
#' better and less error prone to define them upfront. It further allows you
#' to specify specific axidraw options for each pen, e.g. travel speed.
#'
#' @param color The color that this pen corresponds to.
#' @param tip_size The size of the line the pen leaves on the paper in mm
#' @param offset The x and y offset from the starting pen tip
#' @param options An `axi_options` object. See the documentation for
#' [axi_options()] for all the settings.
#'
#' @return An `axi_pen` object
#'
#' @export
#'
#' @examples
#' pen(color = 'forestgreen', tip_size = 2)
#'
#' # Combine multiple pens
#' c(
#'   pen(color = 'forestgreen', tip_size = 2),
#'   pen(color = 'steelblue', tip_size = 1,
#'       options = axi_options(speed_down = 15))
#' )
#'
pen <- function(color, tip_size = NULL, offset = c(0, 0), options = NULL) {
  if (missing(color)) {
    stop('Pens must have a color', call. = FALSE)
  }
  if (!is.null(options) && !is_axi_options(options)) {
    stop('Options must be specified with `axi_options()`', call. = FALSE)
  }
  pen <- list(list(color = color, tip_size = tip_size, offset = offset, options = options))
  class(pen) <- 'axi_pen'
  pen
}
is_pen <- function(x) inherits(x, 'axi_pen')

#' @export
c.axi_pen <- function(..., recursive = FALSE) {
  pens <- list(...)
  if (!all(vapply(pens, is_pen, logical(1)))) {
    stop('Pens can only be combined with other pens', call. = FALSE)
  }
  pens <- unlist(pens, recursive = FALSE)
  class(pens) <- 'axi_pen'
  pens
}

#' @export
print.axi_pen <- function(x, ...) {
  first <- TRUE
  for (pen in x) {
    if (!first) {
      cli::cli_text('* * *')
    }
    first <- FALSE
    col_fmt <- cli::make_ansi_style(pen$color, bg = TRUE)
    cli::cli_text("{.strong Color}..... {col_fmt(' ')}{col_fmt(' ')} ({pen$color})")
    cli::cli_text("{.strong Tip}....... {pen$tip_size} mm")
    cli::cli_text("{.strong Offset}.... x: {pen$offset[1]} mm, y: {pen$offset[2]} mm")
  }
}

validate_pens <- function(pens, first_col, default_tip, default_opt) {
  if (length(pens) == 0) {
    return(pen(first_col, default_tip, options = default_opt))
  }
  if (!is_pen(pens)) {
    stop('Pens must be created with the `pen()` function', call. = FALSE)
  }
  all_pens <- lapply(pens, function(pen) {
    if (is.null(pen$color)) {
      stop('Pens must have a color', call. = FALSE)
    }
    if (is.null(pen$tip_size)) {
      pen$tip_size <- default_tip
    }
    if (is.null(pen$options)) {
      pen$options <- default_opt
    }
    pen
  })
  class(all_pens) <- 'axi_pen'
  colors <- rgb(t(col2rgb(vapply(all_pens, `[[`, character(1), 'color'))), maxColorValue = 255L)
  if (anyDuplicated(colors)) {
    stop("Multiple pens with the same color specified", call. = FALSE)
  }
  first_col <- rgb(t(col2rgb(first_col)), maxColorValue = 255L)
  if (first_col %in% colors) {
    warning('Dropping pen with same color as start configuration')
    all_pens <- all_pens[first_col != colors]
  }
  all_pens <- c(pen(first_col, default_tip, options = default_opt), all_pens)
  colors <- vapply(all_pens, `[[`, character(1), 'color')
  colors <- rgb(t(col2rgb(colors)), maxColorValue = 255L)
  names(all_pens) <- colors
  all_pens
}
