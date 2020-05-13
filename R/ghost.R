#' A mock of a manual axidraw connection
#'
#' This function creates a mock of the manual axidraw connection with the same
#' methods. The mock will collect all the instructions and provides a `preview()`
#' method for inspecting the look of the output. As such it can be used to debug
#' or test code without being connected to an AxiDraw.
#'
#' @param units Either `'cm'` or `'in'` giving the unit the instructions should
#' be interpreted in.
#' @param paper The size of the paper to plot on. Is only relevant for the plot
#' method.
#'
#' @section Instructions:
#' The following methods are available to the returned object:
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
#'
#' @return An object with the methods given in the *Instructions* section.
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
    preview = function(pen_colour = 'black', plot_air = FALSE, air_colour = 'red', size = 1, paper_colour = 'white', background = 'grey') {
      path <- self$get_path()
      ids <- rle(path$raised)
      path$id <- rep(seq_along(ids$lengths), ids$lengths)
      colour <- ifelse(ids$values, air_colour, pen_colour)
      if (!plot_air) {
        colour <- colour[!ids$values]
        path <- path[!path$raised, ]
      }
      grid::grid.newpage()
      grid::grid.rect(gp = grid::gpar(fill = background, colour = NA))
      grid::grid.rect(x = grid::unit(0, 'npc'), y = grid::unit(1, 'npc'),
                      width = grid::unit(private$paper[2], 'mm'),
                      height = grid::unit(private$paper[1], 'mm'),
                      just = c('left', 'top'),
                      gp = grid::gpar(fill = paper_colour, colour = NA))
      grid::grid.polyline(
        x = grid::unit(path$x, private$unit),
        y = grid::unit(1, 'npc') - grid::unit(path$y, private$unit),
        id = path$id,
        gp = grid::gpar(col = colour, lwd = size)
      )
      invisible(self)
    },
    get_path = function() {
      ind <- seq_len(private$current_length)
      data.frame(
        x = private$path$x[ind],
        y = private$path$y[ind],
        raised = private$path$raised[ind]
      )
    }
  ),
  private = list(
    pen_is_up = TRUE,
    unit = NULL,
    paper = NULL,
    path = list(
      x = 0,
      y = 0,
      raised = TRUE
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
        raised = c(private$path$raised, rep(NA, add_rows))
      )
      private$full_length <- length(private$path$x)
    },
    last_row = function() {
      list(
        x = private$path$x[private$current_length],
        y = private$path$y[private$current_length],
        raised = private$path$raised[private$current_length]
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
      private$current_length <- private$current_length + nrows
    }
  )
)

#' @export
plot.AxiGhost <- function(x, y, ...) {
  x$preview(...)
}
