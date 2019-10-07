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
#' - `goto()`: Move the plotter head to the absolute (x, y) position.
#' - `moveto()`: Move the plotter head to the absolute (x, y) position, raising
#'   the head if not already up.
#' - `lineto()`: Move the plotter head to the absolute (x, y) position, lowering
#'   the head if not already down.
#' - `go()`: Move the plotter head with the relative (x, y) amount.
#' - `move()`: Move the plotter head with the relative (x, y) amount, raising
#'   the head if not already up.
#' - `line()`: Move the plotter head with the relative (x, y) amount, lowering
#'   the head if not already down.
#' - `penup()`: Raise the pen head.
#' - `pendown()`: Lower the pen head.
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
  axidraw
}
