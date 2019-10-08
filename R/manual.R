#' Manually control the AxiDraw
#'
#' This function sets up a connection to the AxiDraw and returns an object with
#' methods for directly controlling the movement of the pen plotter.
#'
#' @param options A list of options for the AxiDraw. Use [manual_options()] for
#' help with creating a valid list of options.
#'
#' @note
#' Before and after use it is necessary to call the `connect()`/`disconnect()`
#' methods to signal the start and end of a sequence of instructions.
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
axi_manual <- function(options = list()) {
  axidraw <- import_axidraw()
  for (opt in names(options)) {
    axidraw$options[[opt]] <- options[[opt]]
  }
  axidraw$interactive()
  AxiManual$new(axidraw)
}

AxiManual <- R6::R6Class('AxiManual',
  public = list(
    initialize = function(axidraw) {
      private$axidraw <- axidraw
    },
    print = function(...) {
      cat("An AxiDraw connection. See `?axi_manual` for information about its methods\n")
    },
    connect = function() {
      private$axidraw$connect()
      invisible(self)
    },
    disconnect = function() {
      private$axidraw$disconnect()
      invisible(self)
    },
    go_to = function(x, y) {
      private$axidraw$goto(x, y)
      invisible(self)
    },
    move_to = function(x, y) {
      private$axidraw$moveto(x, y)
      invisible(self)
    },
    line_to = function(x, y) {
      private$axidraw$lineto(x, y)
      invisible(self)
    },
    go_rel = function(x, y) {
      private$axidraw$go(x, y)
      invisible(self)
    },
    move_rel = function(x, y) {
      private$axidraw$move(x, y)
      invisible(self)
    },
    line_rel = function(x, y) {
      private$axidraw$line(x, y)
      invisible(self)
    },
    pen_up = function() {
      private$axidraw$penup()
      invisible(self)
    },
    pen_down = function() {
      private$axidraw$pendown()
      invisible(self)
    }
  ),
  private = list(
    axidraw = NULL
  )
)
