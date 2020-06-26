#' Setting options for AxiDraw
#'
#' This function encapsulates defining general settings for the AxiDraw. Input
#' will be checked against valid values.
#'
#' @param speed_down The maximum movement speed while the pen is lowered as a
#' percentage of the maximum cariage speed. Values must lie between `1` and
#' `110`. Unless `const_speed = TRUE` the carriage uses a smooth accelaration
#' meaning that the maximum speed may only be reached during long straight
#' draws. There is little gain in plotting time when increasing this value, but
#' it may adversely affect quality.
#' @param speed_up As `speed_down` but for travel when the pen is raised.
#' Movement when the pen is raised will always use acceleration and increasing
#' this value can have a large effect and plotting time in cases where the pen
#' jumps around a lot.
#' @param acceleration Gives the acceleration and decelaretion speed as a
#' percentage of the maximum rate. Values must lie between `1` and `100`.
#' Changing this value will not affect the maximum speed, but how fast it will
#' reach it.
#' @param position_down The height of the pen when lowered as a percentage of
#' vertical travel. Value must lie between `1` and `100`.
#' @param position_up As `position_down` but setting the height of the pen when
#' raised.
#' @param rate_lower The rate of vertical movement when the pen is lowered given
#' as a percentage of maximum vertical travel speed. Value must lie between `1`
#' and `100`.
#' @param rate_raise As `rate_lower` but for specifying relative speed when
#' raising the pen.
#' @param delay_down A delay in ms before commencing movement after the pen has
#' been lowered. Value must lie between `-500` and `500`. A delay is
#' automatically applied based on the position and rate, but this can be
#' modified with this setting.
#' @param delay_up As `delay_down` but for commencing movement after the pen has
#' been raised.
#' @param const_speed A boolean given whether the carriage should travel with
#' maximum speed while the pen is down, or use the smooth acceleration.
#' @param model The AxiDraw model to use. This will affect the travel limits
#' imposed on the carriage. Valid values are `'A4'`, `'A3'`, or `'XL'`. If
#' `NULL` the value will be taken from the `'fawkes.model'` option or `'A4'` if
#' missing.
#' @param port The port connection to use. If `NULL` it will be resolved
#' automatically.
#' @param ignore_port If set to `TRUE` it will always communicate with the
#' first device located even if it is not the one given by `port`
#'
#' @return An `axi_options` object
#'
#' @export
axi_options <- function(speed_down = 25, speed_up = 75, acceleration = 75,
                        position_down = 40, position_up = 60, rate_lower = 50,
                        rate_raise = 75, delay_down = 0, delay_up = 0,
                        const_speed = FALSE, model = NULL, port = NULL,
                        ignore_port = FALSE) {
  opt <- list()
  opt$speed_pendown <- check_range(speed_down, 1, 110)
  opt$speed_penup <- check_range(speed_up, 1, 110)
  opt$accel <- check_range(acceleration, 1, 100)
  opt$pen_pos_down <- check_range(position_down, 0, 100)
  opt$pen_pos_up <- check_range(position_up, 0, 100)
  opt$pen_rate_lower <- check_range(rate_lower, 1, 100)
  opt$pen_rate_raise <- check_range(rate_raise, 1, 100)
  opt$pen_delay_down <- check_range(delay_down, -500, 500)
  opt$pen_delay_up <- check_range(delay_up, -500, 500)
  opt$const_speed <- check_flag(const_speed)
  if (is.null(model)) model <- getOption('fawkes.model', 'A4')
  opt$model <- match(tolower(model), c('a4', 'a3', 'xl'))
  opt$port <- check_string_or_null(port)
  opt$port_config <- if (ignore_port) 1L else 0L
  class(opt) <- 'axi_options'
  opt
}
is_axi_options <- function(x) inherits(x, 'axi_options')

set_options <- function(axidraw, options) {
  if (!is_axi_options(options)) {
    stop('options must be specified with `axi_options()`', call. = FALSE)
  }
  for (opt in names(options)) {
    axidraw$options[[opt]] <- options[[opt]]
  }
  axidraw
}

check_range <- function(value, lower, upper, integer = TRUE) {
  name <- deparse(substitute(value))
  if (length(value) != 1) {
    stop(name, ' must be a scalar', call. = FALSE)
  }
  if (value < lower || value > upper) {
    stop(name, ' must be between ', lower, ' and ', upper, call. = FALSE)
  }
  if (integer) {
    as.integer(value)
  } else {
    as.numeric(value)
  }
}
check_flag <- function(value) {
  name <- deparse(substitute(value))
  if (!is.logical(value) || length(value) != 1) {
    stop(name, ' must be a scalar boolean', call. = FALSE)
  }
  as.logical(value)
}
check_string_or_null <- function(value) {
  name <- deparse(substitute(value))
  if (is.null(value)) return(value)
  if (!is.character(value) || length(value) != 1) {
    stop(name, ' must be a string', call. = FALSE)
  }
  as.character(value)
}
