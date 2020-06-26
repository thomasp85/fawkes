#' An asynchronous version of a manual axidraw connection
#'
#' This function creates a version of a manual axidraw connection that instead
#' of sending instructions to the plotter will collect them for later replay.
#' For bigger plots it is preferred to use this as it makes it easier to pause
#' and rewind if something goes wrong.
#'
#' @param units Either `'cm'` or `'in'` giving the unit the instructions should
#' be interpreted in.
#' @param paper The size of the paper to plot on. Is only relevant for the plot
#' method.
#'
#' @section Recording Instructions:
#' The following methods are available for recording drawing instructions and
#' mirror those of [axi_manual()]:
#' - `connect()`: Start a connection to the plotter.
#' - `disconnect()`: End a connection to the plotter.
#' - `go_to()`: Move the plotter head to the absolute (x, y) position.
#' - `move_to()`: Move the plotter head to the absolute (x, y) position, raising
#'   the head if not already up.
#' - `line_to()`: Move the plotter head to the absolute (x, y) position, lowering
#'   the head if not already down.
#' - `go_rel()`: Move the plotter head with the relative (x, y) amount.
#' - `move_rel()`: Move the plotter head with the relative (x, y) amount, raising
#'   the head if not already up.
#' - `line_rel()`: Move the plotter head with the relative (x, y) amount, lowering
#'   the head if not already down.
#' - `pen_up()`: Raise the pen head.
#' - `pen_down()`: Lower the pen head.
#' - `set_pen_color()`: Records the color of the pen currently in use.
#'
#' @section After Recording:
#' Once the instructions have been recorded there is a bunch of stuff you can do
#' with it:
#' - `plot()`: Will take the recorded data and send it to the plotter, for real
#'   this time. During plotting an internal counter will keep track of where you
#'   are so that you can pause and resume the plot at any time.
#' - `preview()`: Will render the plotter movement on screen. You can choose to
#'   only show drawing movement, or air movement as well. Further, you can
#'   either set a single color for the drawing lines or use the color encoded
#'   with the movement. Lastly, it is possible to draw all movement or either
#'   the remaining or already drawn parts, which uses the internal counter to
#'   figure out how for proceeded.
#' - `get_pen_position()`: Get the internal counter. The counter records
#'   everything between a pen down and a pen up as a single movement.
#' - `set_pen_position()`: Sets a new pen position manually.
#' - `rewind_pen_postion()`: Move the pen position backwards in time.
#' - `forward_pen_position()`: Move the pen position forward in time.
#' - `reset_pen_position()`: Sets the pen position to the start of the plot.
#' - `get_path()`: Returns the recorded movement information.
#'
#' @return An object with the methods given in the *Recording Instructions* and
#' *After Recording* sections.
#'
#' @export
axi_ghost <- function(units = 'cm', paper = 'A4') {
  units <- match.arg(tolower(units), c('cm', 'in'))
  paper <- paper_dimensions(paper, TRUE)
  AxiGhost$new(units, paper)
}

AxiGhost <- R6::R6Class('AxiGhost',
  public = list(
    initialize = function(unit, paper) {
      private$unit <- unit
      private$paper <- paper
      private$increase_alloc(private$full_length)
    },
    print = function(...) {
      cat("An AxiGhost object, mimicking the functionality of AxiManual. See `?axi_ghost` for information about its methods\n")
    },
    connect = function() {
      invisible(self)
    },
    disconnect = function() {
      invisible(self)
    },
    update_options = function(options) {
      invisible(self)
    },
    go_to = function(x, y) {
      private$append_row(x, y, private$pen_is_up)
      invisible(self)
    },
    move_to = function(x, y) {
      self$pen_up()
      private$append_row(x, y, TRUE)
      invisible(self)
    },
    line_to = function(x, y) {
      self$pen_down()
      private$append_row(x, y, FALSE)
      invisible(self)
    },
    go_rel = function(x, y) {
      loc <- private$last_row()
      private$append_row(loc$x + cumsum(x), loc$y + cumsum(y), private$pen_is_up)
      invisible(self)
    },
    move_rel = function(x, y) {
      self$pen_up()
      loc <- private$last_row()
      private$append_row(loc$x + cumsum(x), loc$y + cumsum(y), TRUE)
      invisible(self)
    },
    line_rel = function(x, y) {
      self$pen_down()
      loc <- private$last_row()
      private$append_row(loc$x + cumsum(x), loc$y + cumsum(y), FALSE)
      invisible(self)
    },
    pen_up = function() {
      if (!private$pen_is_up) {
        private$pen_is_up <- TRUE
        loc <- private$last_row()
        private$append_row(loc$x, loc$y, TRUE)
      }
      invisible(self)
    },
    pen_down = function() {
      if (private$pen_is_up) {
        private$pen_is_up <- FALSE
        loc <- private$last_row()
        private$append_row(loc$x, loc$y, FALSE)
      }
      invisible(self)
    },
    set_pen_color = function(color, pen, ...) {
      private$pen_has_color <- TRUE
      private$current_color <- color
      private$pens[[color]] <- pen
      invisible(self)
    },
    preview = function(plot_air = FALSE, air_color = 'red', pen_color = NULL, size = 1, paper_color = 'white', background = 'grey', show = 'all') {
      show <- match.arg(show, c('all', 'drawn', 'remaining'))
      path <- self$get_path()
      ids <- rle(path$raised)
      path$id <- rep(seq_along(ids$lengths), ids$lengths)
      if (!is.null(pen_color)) path$color <- pen_color
      if (plot_air) {
        path$color[path$raised] <- air_color
      } else {
        path <- path[!path$raised, , drop = FALSE]
      }
      if (show == 'drawn') {
        pen_id <- unique(path$id[!path$raised])[private$pen_position]
        pen_ind <- match(pen_id, path$id)
        path <- path[seq_len(pen_ind - 1), , drop = FALSE]
      } else if (show == 'remaining') {
        pen_id <- unique(path$id[!path$raised])[private$pen_position]
        pen_ind <- match(pen_id, path$id)
        path <- path[seq(pen_ind, nrow(path)), , drop = FALSE]
      }
      color <- path$color[cumsum(rle(path$id)$lengths) - 1]
      grid::grid.newpage()
      grid::grid.rect(gp = grid::gpar(fill = background, col = NA))
      grid::grid.rect(x = grid::unit(0, 'npc'), y = grid::unit(1, 'npc'),
                      width = grid::unit(private$paper[2], 'mm'),
                      height = grid::unit(private$paper[1], 'mm'),
                      just = c('left', 'top'),
                      gp = grid::gpar(fill = paper_color, col = NA))
      grid::grid.polyline(
        x = grid::unit(path$x, private$unit),
        y = grid::unit(1, 'npc') - grid::unit(path$y, private$unit),
        id = path$id,
        gp = grid::gpar(col = color, lwd = size)
      )
      invisible(self)
    },
    plot = function(reset = FALSE) {
      ad <- axi_manual()
      ad$connect()

      if (reset) private$pen_location <- 1L

      path <- self$get_path()
      ids <- rle(path$raised)
      path$id <- rep(seq_along(ids$lengths), ids$lengths)
      path <- path[!path$raised, , drop = FALSE]
      path <- split(path, path$id)
      if (private$pen_location > length(path)) {
        message("Nothing to plot. Try resetting the pen location")
        return(invisible(self))
      }
      path  <- path[seq(private$pen_location, length(path))]

      current_color <- ''
      on.exit({
        ad$move_to(0, 0)
        ad$disconnect()
      })
      for (p in path) {
        if (p$color[1] != current_color && private$pen_has_color) {
          ad$move_to(0, 0)
          col_fmt <- cli::make_ansi_style(p$color[1], bg = TRUE)
          cli::cli_alert_warning("Please change pen color")
          cli::cli_alert_info("New color: {col_fmt('  ')} ({p$color[1]})")
          readline()
          ad$update_options(private$pens[[p$color[1]]]$options)
          current_color <- p$color[1]
        }
        ad$move_to(p$x[1], p$y[1])
        for (i in seq_along(p$x)[-1]) {
          ad$line_to(p$x[i], p$y[i])
        }
        ad$pen_up()
        private$pen_location <- private$pen_location + 1L
      }
    },
    get_pen_position = function() {
      private$pen_position
    },
    rewind_pen_position = function(by = 1L) {
      private$pen_position <- private$pen_position - as.integer(by)
    },
    forward_pen_position = function(by = 1L) {
      private$pen_position <- private$pen_position + as.integer(by)
    },
    reset_pen_position = function() {
      private$pen_position <- 1L
    },
    set_pen_position = function(at) {
      private$pen_position <- as.integer(at)
    },
    get_path = function() {
      ind <- seq_len(private$current_length)
      data.frame(
        x = private$path$x[ind],
        y = private$path$y[ind],
        raised = private$path$raised[ind],
        color = private$path$color[ind]
      )
    }
  ),
  private = list(
    pen_location = 1L,
    pen_has_color = FALSE,
    current_color = NA_character_,
    pen_is_up = TRUE,
    pens = list(),
    unit = NULL,
    paper = NULL,
    path = list(
      x = 0,
      y = 0,
      raised = TRUE,
      color = NA_character_
    ),
    current_length = 1L,
    full_length = 100L,

    increase_alloc = function(minimum = NULL) {
      n <- ceiling(length(private$path$x) * 2)
      if (n < minimum) n <- minimum
      add_rows <- n - length(private$path$x)
      private$path <- list(
        x = c(private$path$x, rep(NA_real_, add_rows)),
        y = c(private$path$y, rep(NA_real_, add_rows)),
        raised = c(private$path$raised, rep(NA, add_rows)),
        color = c(private$path$color, rep(NA_character_, add_rows))
      )
      private$full_length <- length(private$path$x)
    },
    last_row = function() {
      list(
        x = private$path$x[private$current_length],
        y = private$path$y[private$current_length],
        raised = private$path$raised[private$current_length],
        color = private$path$color[private$current_length]
      )
    },
    append_row = function(x, y, raised) {
      nrows <- length(x)
      if (private$current_length + nrows > private$full_length) {
        private$increase_alloc(nrows)
      }
      ind <- private$current_length + seq_len(nrows)
      private$path$x[ind] <- x
      private$path$y[ind] <- y
      private$path$raised[ind] <- raised
      private$path$color[ind] <- private$current_color
      private$current_length <- private$current_length + nrows
    }
  )
)

#' @export
plot.AxiGhost <- function(x, y, ...) {
  x$preview(...)
}
