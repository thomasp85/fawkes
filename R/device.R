#' A graphic device that renders using the AxiDraw pen plooter
#'
#' `axi_dev()` opens up a graphic device that takes plotting instructions from
#' e.g. `plot()` or ggplot2, and renders it with the AxiDraw. `ghost_dev()`
#' behaves like `axi_dev()`, but instead of sending instructions to the plotter
#' it will collect them and allow you to preview the movement of the pen.
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
#' @param margins The margins of the paper, in mm. The spec follows that of css,
#' meaning that if it is given as a single value it defines all margins, if
#' given as two values it defines top/bottom, left/right, if it is given as
#' three values it defines top, left/right, bottom, and if it is given as four
#' values it defines top, right, bottom, left.
#' @param tip_size The size of the tip (i.e. the width of the line it draws) of
#' the initial pen
#' @param color The color of the initial pen
#' @param ignore_color Logical. Should changes in color output be ignored? If
#' `TRUE` the plotter will not pause for pen change when the colour changes
#' @param ignore_lwd Logical. Should the device ignore the lwd and also just
#' draw lines as a single pen stroke?
#' @param line_overlap The overlap between adjacent pen strokes when filling out
#' shapes and drawing thick lines, in mm. Setting this to a negative amount will
#' cause gaps between the lines.
#' @param draw_fill Logical. Should fill be drawn using hatching?
#' @param hatch_angle Angle in degrees that the hatching of fill should be drawn
#' with.
#' @param optimize_order Logical. Should the drawing order be optimised so that
#' pen movement is minimised? Will only affect consecutive calls to the same
#' drawing primitive.
#' @param options An `axi_options` object. See the documentation for
#' [axi_options()] for all the settings.
#'
#' @importFrom grDevices col2rgb rgb
#' @importFrom devout rdevice
#' @export
#'
axi_dev <- function(paper_size = "A4", portrait = TRUE, margins = 20, tip_size = 1,
                    color = 'black', ignore_color = TRUE, ignore_lwd = FALSE,
                    line_overlap = 0.1, draw_fill = TRUE, hatch_angle = 45,
                    optimize_order = 'all', options = axi_options()) {
  paper_size <- paper_dimensions(paper_size, portrait)
  margins <- expand_margins(margins)
  size <- paper_size - c(margins[2] + margins[4], margins[1] + margins[3])
  if (any(size <= 0)) {
    rlang::abort("margins larger than the paper size")
  }
  color <- as.vector(col2rgb(color, TRUE))
  optimize_order <- match.arg(optimize_order, c('none', 'primitive', 'all'))
  axidraw <- axi_manual(units = 'cm', options)
  info <- list2env(list(
    ad = axidraw, size = size, flip = portrait, offset = margins[c(4, 1)],
    p_width = paper_size[1], tip_size = tip_size, color = color,
    ignore_color = ignore_color, ignore_lwd = ignore_lwd,
    line_overlap = line_overlap, draw_fill = draw_fill, hatch_angle = hatch_angle,
    optimize_order = optimize_order, collection = list(), current_primitive = '',
    first_page = TRUE
  ), parent = emptyenv())
  rdevice("axidraw_callback", info = info)
}
#' @rdname axi_dev
#' @export
ghost_dev <- function(paper_size = "A4", portrait = TRUE, margins = 20, tip_size = 1,
                     color = 'black', ignore_color = TRUE, ignore_lwd = FALSE,
                     line_overlap = 0.1, draw_fill = TRUE, hatch_angle = 45,
                     optimize_order = 'all', options) {
  axidraw <- axi_ghost(units = 'cm', paper = paper_size)
  paper_size <- paper_dimensions(paper_size, portrait)
  margins <- expand_margins(margins)
  size <- paper_size - c(margins[2] + margins[4], margins[1] + margins[3])
  if (any(size <= 0)) {
    rlang::abort("margins larger than the paper size")
  }
  color <- as.vector(col2rgb(color, TRUE))
  optimize_order <- match.arg(optimize_order, c('none', 'primitive', 'all'))
  info <- list2env(list(
    ad = axidraw, size = size, flip = portrait, offset = margins[c(4, 1)],
    p_width = paper_size[1], tip_size = tip_size, color = color,
    ignore_color = ignore_color, ignore_lwd = ignore_lwd,
    line_overlap = line_overlap, draw_fill = draw_fill, hatch_angle = hatch_angle,
    optimize_order = optimize_order, collection = list(), current_primitive = '',
    first_page = TRUE
  ), parent = emptyenv())
  rdevice("axidraw_callback", info = info)
  invisible(axidraw)
}
#' Callbacks for the fawkes devices
#'
#' @keywords internal
#' @export
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
  state$dd$right <- state$rdata$info$size[1]
  state$dd$bottom <- state$rdata$info$size[2]
  state$dd$clipRight <- state$rdata$info$size[1]
  state$dd$clipBottom <- state$rdata$info$size[2]
  state$dd$ipr <- c(0.03937, 0.03937)
  state$rdata$info$last_primitive <- NA
  state$rdata$info$calls <- list()
  state$rdata$info$ad$connect()
  state
}
.axi_close <- function(args, state) {
  draw_collection(state, NA)
  state$rdata$info$ad$move_to(0, 0)
  state$rdata$info$ad$disconnect()
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
  state$rdata$info$ad$move_to(0, 0)
  if (!state$rdata$info$first_page) {
    cli::cli_alert_warning("Change paper (press enter when ready for next page)")
    readline()
  }
  state$rdata$info$first_page <- FALSE
  state
}
.axi_circle <- function(args, state) {
  n_points <-  round((2 * pi) / acos((args$r - 0.5) / args$r)) * 10;
  angles <- seq(0, 2*pi, length.out = n_points + 1)[-(n_points + 1)]
  shape <- list(
    x = cos(angles) * args$r + args$x,
    y = sin(angles) * args$r + args$y
  )

  # Fill
  if (has_fill(state)) {
    state <- update_fill(state, 'circle')
    fill <- create_circle_fill(args$x, args$y, args$r, angles, state)
    state <- collect_lines(fill, state)
  }

  # Stroke
  if (has_stroke(state)) {
    state <- update_stroke(state, 'circle')
    stroke <- create_closed_stroke(list(shape), state)
    state <- collect_lines(stroke, state)
  }

  state
}
.axi_rect <- function(args, state) {
  shape <- list(
    x = c(args$x0, args$x1)[c(1, 2, 2, 1)],
    y = c(args$y0, args$y1)[c(1, 1, 2, 2)]
  )

  # Fill
  if (has_fill(state)) {
    state <- update_fill(state, 'rect')
    fill <- create_fill(shape, state)
    state <- collect_lines(fill, state)
  }

  # Stroke
  if (has_stroke(state)) {
    state <- update_stroke(state, 'rect')
    stroke <- create_closed_stroke(list(shape), state)
    state <- collect_lines(stroke, state)
  }

  state
}
.axi_line <- function(args, state) {
  if (has_stroke(state)) {
    line <- list(
      x = c(args$x1, args$x2),
      y = c(args$y1, args$y2)
    )

    state <- update_stroke(state, 'line')
    stroke <- create_open_stroke(list(line), state)
    state <- collect_lines(stroke, state)
  }
  state
}
.axi_polyline <- function(args, state) {
  if (has_stroke(state)) {
    line <- list(
      x = args$x,
      y = args$y
    )

    state <- update_stroke(state, 'polyline')
    stroke <- create_open_stroke(list(line), state)
    state <- collect_lines(stroke, state)
  }
  state
}
.axi_polygon <- function(args, state) {
  shape <- list(
    x = args$x,
    y = args$y
  )

  # Fill
  if (has_fill(state)) {
    state <- update_fill(state, 'polygon')
    fill <- create_fill(list(shape), state)
    state <- collect_lines(fill, state)
  }

  # Stroke
  if (has_stroke(state)) {
    state <- update_stroke(state, 'polygon')
    stroke <- create_closed_stroke(list(shape), state)
    state <- collect_lines(stroke, state)
  }

  state
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
    state <- update_fill(state, 'path')
    fill <- create_fill(path, state)
    state <- collect_lines(fill, state)
  }

  # Stroke
  if (has_stroke(state)) {
    state <- update_stroke(state, 'path')
    stroke <- create_closed_stroke(path, state)
    state <- collect_lines(stroke, state)
  }

  state
}

has_stroke <- function(state) {
  state$gc$lwd != 0 && state$gc$col[4] != 0 # && state$gc$lty != 0
}
has_fill <- function(state) {
  state$rdata$info$draw_fill && state$gc$fill[4] != 0
}
update_fill <- function(state, primitive) {
  if (!can_collect(state, primitive, 1L, list())) {
    state <- draw_collection(state, primitive)
    state <- update_pen(state, state$gc$fill)
  }
  state
}
update_stroke <- function(state, primitive) {
  if (!can_collect(state, primitive, list(), 1L)) {
    state <- draw_collection(state, primitive)
    state <- update_pen(state, state$gc$col)
  }
  state
}
create_closed_stroke <- function(shapes, state) {
  stroke <- state$gc$lwd * 72 / 96
  n_strokes <- ceiling(stroke / (state$rdata$info$tip_size - state$rdata$info$line_overlap))
  if (state$rdata$info$ignore_lwd || stroke <= state$rdata$info$tip_size) {
    clip_closed_stroke(shapes, state)
  } else {
    all_strokes <- lapply(shapes, function(path) {
      full_stroke <- polyclip::polylineoffset(
        path,
        (stroke - state$rdata$info$tip_size) / 2,
        jointype = joins[state$gc$ljoin],
        endtype = 'closedline',
        miterlim = state$gc$lmitre
      )
      full_stroke <- polyclip::polyclip(full_stroke, clip_box(state), 'intersection')
      full_stroke <- lapply(full_stroke, fill_stroke, state$rdata$info$tip_size - state$rdata$info$line_overlap, n_strokes)
      unlist(full_stroke, recursive = FALSE, use.names = FALSE)
    })
    unlist(all_strokes, recursive = FALSE, use.names = FALSE)
  }
}
create_open_stroke <- function(paths, state) {
  stroke <- state$gc$lwd * 72 / 96
  n_strokes <- ceiling(stroke / (state$rdata$info$tip_size - state$rdata$info$line_overlap))
  if (state$rdata$info$ignore_lwd || stroke <= state$rdata$info$tip_size) {
    paths <- lapply(paths, polyclip::polyclip, clip_box(state), 'intersection', closed = FALSE)
    unlist(paths, recursive = FALSE, use.names = FALSE)
  } else {
    all_paths <- lapply(paths, function(path) {
      full_path <- polyclip::polylineoffset(
        path,
        (stroke - state$rdata$info$tip_size) / 2,
        jointype = joins[state$gc$ljoin],
        endtype = ends[state$gc$lend],
        miterlim = state$gc$lmitre
      )
      full_path <- polyclip::polyclip(full_path, clip_box(state), 'intersection')
      full_path <- lapply(full_path, fill_stroke, state$rdata$info$tip_size - state$rdata$info$line_overlap, n_strokes)
      unlist(full_path, recursive = FALSE, use.names = FALSE)
    })
    unlist(all_paths, recursive = FALSE, use.names = FALSE)
  }
}
create_fill <- function(fill, state) {
  fill <- polyclip::polyclip(fill, clip_box(state), 'intersection')
  fill <- fill_shape(fill, state$rdata$info$tip_size, state$rdata$info$line_overlap, state$rdata$info$hatch_angle)
}
create_circle_fill <- function(x, y, r, angles, state) {
  radii <- seq(r - state$rdata$info$tip_size / 2, 0, by = -(state$rdata$info$tip_size - state$rdata$info$line_overlap))
  radii <- unique(c(radii, 0))
  angles <- c(angles, angles[1])
  cos_a <- cos(angles)
  sin_a <- sin(angles)
  bbox <- clip_box(state)
  unlist(lapply(radii, function(r) {
    if (r == 0) {
      cos_a <- c(0, 0)
      sin_a <- c(0, 0)
    }
    polyclip::polyclip(
      list(x = cos_a * r + x, y = sin_a * r + y),
      bbox,
      'intersection'
    )
  }), recursive = FALSE)
}
clip_box <- function(state) {
  list(
    x = c(state$dd$clipLeft - 1e5, state$dd$clipRight + 1e-5)[c(1, 2, 2, 1)],
    y = c(state$dd$clipTop + 1e5, state$dd$clipBottom - 1e-5)[c(1, 1, 2, 2)]
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
fill_shape <- function(shape, tip_width, overlap, angle = 45) {
  shape <- polyclip::polyoffset(shape, -tip_width / 2)
  bbox_x <- range(unlist(lapply(shape, `[[`, 'x')))
  bbox_y <- range(unlist(lapply(shape, `[[`, 'y')))
  max_length <- max(diff(bbox_x), diff(bbox_y))
  x <- rep(seq(-max_length, max_length, by = tip_width - overlap), each = 2)
  y <- rep(c(-max_length, max_length), length.out = length(x))
  y <- c(y[-1], y[1])
  theta <- 2 * pi * angle / 360
  cos_t <- cos(theta)
  sin_t <- sin(theta)
  fill <- list(
    x = x * cos_t - y * sin_t + mean(bbox_x),
    y = y * cos_t + x * sin_t + mean(bbox_y)
  )
  fill <- polyclip::polyclip(list(fill), shape, 'intersection', closed = FALSE)
  shape <- lapply(shape, function(x) {
    x$x <- c(x$x, x$x[1])
    x$y <- c(x$y, x$y[1])
    x
  })
  c(fill, shape)
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

can_collect <- function(state, primitive, fill, stroke) {
  if (state$rdata$info$optimize_order == 'none') return(FALSE)
  if (state$rdata$info$optimize_order == 'primitive' &&
      primitive != state$rdata$info$current_primitive) return(FALSE)
  if (!state$rdata$info$ignore_color &&
      !identical(state$gc$col[1:3], state$rdata$info$color[1:3]) &&
      length(stroke) > 0) return(FALSE)
  if (!state$rdata$info$ignore_color &&
      !identical(state$gc$fill[1:3], state$rdata$info$color[1:3]) &&
      length(fill) > 0) return(FALSE)
  TRUE
}
collect_lines <- function(lines, state) {
  if (length(lines) == 0) return(state)
  start <- c(first(first(lines)$x), first(first(lines)$y))
  end <- c(last(last(lines)$x), last(last(lines)$y))
  state$rdata$info$collection[[length(state$rdata$info$collection) + 1]] <- list(
    start = start,
    end = end,
    lines = lines
  )

  state
}
draw_collection <- function(state, new_primitive) {
  collection <- state$rdata$info$collection
  if (length(collection) != 0) {
    if (state$rdata$info$optimize_order != 'none') {
      collection <- optimize_order(collection)
    }
    lapply(collection, function(shape) {
      draw_lines(shape$lines, state)
    })
    state$rdata$info$collection <- list()
  }
  state$rdata$info$current_primitive <- new_primitive
  state
}
update_pen <- function(state, color) {
  if (!state$rdata$info$ignore_color && any(state$rdata$info$color[1:3] != color[1:3])) {
    state$rdata$info$ad$move_to(0, 0)
    col <- rgb(color[1], color[2], color[3], maxColorValue = 255)
    col_fmt <- cli::make_ansi_style(col, bg = TRUE)
    cli::cli_alert_warning("Please change pen color")
    cli::cli_alert_info("New color: {col_fmt('  ')} ({col})")
    cli::cli_alert_info("Enter new tip size (in mm) or leave blank to keep current {state$rdata$info$tip_size}mm")
    tip_size <- readline()
    if (tip_size != '') {
      while (is.na(as.numeric(tip_size))) {
        cli::cli_alert_warning('Invalid tip size `{tip_size}`. Enter a valid one:')
        tip_size <- readline()
        if (tip_size == '') break
      }
      if (tip_size != '') state$rdata$info$tip_size <- as.numeric(tip_size)
    }
    state$rdata$info$color <- color
  }
  state
}
draw_lines <- function(paths, state) {
  ad <- state$rdata$info$ad
  is_ghost <- inherits(ad, 'AxiGhost')
  for (path in paths) {
    if (length(path) == 0 || length(path$x) == 0) next
    path <- prepare_path(path, state)
    ad$move_to(first(path$x), first(path$y))
    if (is_ghost) {
      ad$line_to(path$x[-1], path$y[-1])
    } else {
      for (i in seq_along(path$x)[-1]) {
        ad$line_to(path$x[i], path$y[i])
      }
    }
  }
  ad$pen_up()
}
prepare_path <- function(path, state) {
  path$x <- path$x + state$rdata$info$offset[1]
  path$y <- path$y + state$rdata$info$offset[2]
  if (state$rdata$info$flip) {
    path$x <- -1 * (path$x - state$rdata$info$p_width)
    path[c('x', 'y')] <- path[c('y', 'x')]
  }
  path$x <- path$x / 10
  path$y <- path$y / 10
  path
}
joins <- c('round', 'mitre', 'bevel')
ends <- c('openround', 'openbutt', 'opensquare')

optimize_order <- function(paths) {
  starts <- do.call(cbind, lapply(paths, `[[`, 'start'))
  ends <- do.call(cbind, lapply(paths, `[[`, 'end'))
  dist <- asym_dist(starts, ends)
  tour <- TSP::solve_TSP(TSP::ATSP(dist))
  paths[as.integer(tour)]
}
