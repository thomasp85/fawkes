#' A graphic device that renders using the AxiDraw pen plooter
#'
#' This call opens up a graphic device that takes plotting instructions from
#' e.g. `plot()` or ggplot2, and renders it with the AxiDraw.
#'
#' At the moment the device will ignore colour information and will only draw
#' the stroke of objects. Further, any linetype information is lost. It will try
#' to match the linewidth, but can obviously not draw something thinner than the
#' tip of the pen. Clipping is, incredibly, supported, but text is not. Most of
#' the restrictions above will be handled with time.
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
    clip = .axi_clip(args, state),
    newPage = .axi_new_page(args, state),
    circle = .axi_circle(args, state),
    rect = .axi_rect(args, state),
    line = .axi_line(args, state),
    polyline = .axi_polyline(args, state),
    polygon = .axi_polygon(args, state),
    path = .axi_path(args, state)
  )
  state
}

.axi_open <- function(args, state) {
  state$dd$right <- state$size[1]
  state$dd$bottom <- state$size[2]
  state$dd$clipRight <- state$size[1]
  state$dd$clipBottom <- state$size[2]
  state$dd$ipr <- c(0.03937, 0.03937)
  state$last_primitive <- NA
  state$calls <- list()
  state$ad$connect()
  state
}
.axi_close <- function(args, state) {
  state$dd$disconnect()
  state
}
.axi_clip <- function(args, state) {
  state$dd$clipLeft <- args$x0
  state$dd$clipRight <- args$x1
  state$dd$clipTop <- args$y0
  state$dd$clipBottom <- args$y1
  state
}
.axi_new_page <- function(args, state) {
  state$ad$move_to(0, 0)
  readline("Change paper (press enter when ready for next page) ")
}
.axi_circle <- function(args, state) {
  n_points <-  round((2 * pi) / acos((args$r - 0.5) / args$r));
  angles <- seq(0, 2*pi, length.out = n_points + 1)[-(n_points + 1)]
  shape <- list(
    x = cos(angles) * args$r + args$x,
    y = sin(angles) * args$r + args$y
  )

  # Fill
  if (has_fill(state)) {

  }

  # Stroke
  if (has_stroke(state)) {
    create_closed_stroke(list(shape), state)
  }
}
.axi_rect <- function(args, state) {
  shape <- list(
    x = c(args$x0, args$x1)[c(1, 2, 2, 1)],
    y = c(args$y0, args$y1)[c(1, 1, 2, 2)]
  )

  # Fill
  if (has_fill(state)) {

  }

  # Stroke
  if (has_stroke(state)) {
    create_closed_stroke(list(shape), state)
  }
}
.axi_line <- function(args, state) {
  if (has_stroke(state)) {
    line <- list(
      x = c(args$x1, args$x2),
      y = c(args$y1, args$y2)
    )
    create_open_stroke(list(line), state)
  }
}
.axi_polyline <- function(args, state) {
  if (has_stroke(state)) {
    line <- list(
      x = args$x,
      y = args$y
    )
    create_open_stroke(list(line), state)
  }
}
.axi_polygon <- function(args, state) {
  shape <- list(
    x = args$x,
    y = args$y
  )

  # Fill
  if (has_fill(state)) {

  }

  # Stroke
  if (has_stroke(state)) {
    create_closed_stroke(list(shape), state)
  }
}
.axi_path <- function(args, state) {
  path_id <- rep(seq_len(args$npoly), args$nper)
  x <- split(args$x, path_id)
  y <- split(args$y, path_id)
  path <- lapply(seq_along(x), function(i) {
    list(x = x[[i]], y = y[[i]])
  })
  # Fill
  if (has_fill(state)) {

  }

  # Stroke
  if (has_stroke(state)) {
    create_closed_stroke(path, state)
  }
}

has_stroke <- function(state) {
  state$gd$lwd != 0 && state$gd$col[4] != 0 && state$gd$lty != 0
}
has_fill <- function(state) {
  state$draw_fill && state$gd$fill[4 != 0]
}
create_closed_stroke <- function(shapes, state) {
  stroke <- state$gc$lwd * 72 / 96
  n_strokes <- ceiling(stroke / state$tip_size)
  if (n_strokes == 1) {
    clip_closed_stroke(shapes, state)
  } else {
    all_strokes <- lapply(shapes, function(path) {
      full_stroke <- polyclip::polylineoffset(
        path,
        (stroke - state$tip_size) / 2,
        jointype = joins[state$gc$ljoin],
        endtype = 'closedline',
        miterlim = state$gc$lmitre
      )
      full_stroke <- polyclip::polyclip(full_stroke, clip_box(state), 'intersection')
      full_stroke <- lapply(full_stroke, fill_stroke, state$tip_size, n_strokes)
      unlist(full_stroke, recursive = FALSE, use.names = FALSE)
    })
    unlist(all_strokes, recursive = FALSE, use.names = FALSE)
  }
}
create_open_stroke <- function(paths, state) {
  stroke <- state$gc$lwd * 72 / 96
  n_strokes <- ceiling(stroke / state$tip_size)
  if (n_strokes == 1) {
    paths <- lapply(paths, polyclip::polyclip, clip_box(state), 'intersection', closed = FALSE)
    unlist(paths, recursive = FALSE, use.names = FALSE)
  } else {
    all_paths <- lapply(paths, function(path) {
      full_path <- polyclip::polylineoffset(
        path,
        (stroke - state$tip_size) / 2,
        jointype = joins[state$gc$ljoin],
        endtype = ends[state$gc$lend],
        miterlim = state$gc$lmitre
      )
      full_path <- polyclip::polyclip(full_path, clip_box(state), 'intersection')
      full_path <- lapply(full_path, fill_stroke, state$tip_size, n_strokes)
      unlist(full_path, recursive = FALSE, use.names = FALSE)
    })
    unlist(all_paths, recursive = FALSE, use.names = FALSE)
  }
}
clip_box <- function(state) {
  list(
    x = c(state$dd$clipLeft, state$dd$clipRight)[c(1, 2, 2, 1)],
    y = c(state$dd$clipTop, state$dd$clipBottom)[c(1, 1, 2, 2)]
  )
}
fill_stroke <- function(outline, stroke_width, n_strokes) {
  dilations <- seq(stroke_width, by = stroke_width, length.out = (n_strokes - 2) / 2)
  if (n_strokes %% 2 != 0) {
    last(dilations) <- last(dilations) - stroke_width * 0.5
  }
  c(
    list(outline),
    unlist(
      lapply(-dilations, polyclip::polyoffset, A = outline, miterlim = 10000, jointype = 'miter'),
      recursive = FALSE,
      use.names = FALSE
    )
  )
}
clip_closed_stroke <- function(shape, state) {
  cb <- clip_box(state)
  strokes <- lapply(shape, function(path) {
    path$x <- c(path$x, path$x[1])
    path$y <- c(path$y, path$y[1])
    path <- polyclip::polyclip(path, cb, 'intersection', closed = FALSE)
    if (length(path) == 1) return(path)
    for (i in rev(seq_len(length(path) - 1))) {
      if (isTRUE(all.equal(last(path[[i]]$x), first(path[[i + 1]]$x))) &&
          isTRUE(all.equal(last(path[[i]]$y), first(path[[i + 1]]$y)))) {
        path[[i]]$x <- c(path[[i]]$x, path[[i + 1]]$x[-1])
        path[[i]]$y <- c(path[[i]]$y, path[[i + 1]]$y[-1])
        path[i + 1] <- NULL
      }
      if (isTRUE(all.equal(last(last(path)$x), first(first(path)$x))) &&
          isTRUE(all.equal(last(last(path)$y), first(first(path)$y)))) {
        first(path)$x <- c(last(path)$x, first(path)$x[-1])
        first(path)$y <- c(last(path)$y, first(path)$y[-1])
        path[length(path)] <- NULL
      }
    }
    path
  })
  unlist(strokes, recursive = FALSE, use.names = FALSE)
}
joins <- c('round', 'mitre', 'bevel')
ends <- c('openround', 'openbutt', 'opensquare')
