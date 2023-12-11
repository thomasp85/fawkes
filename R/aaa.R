#' Install the AxiDraw python API
#'
#' This function will download and install the AxiDraw python API for use with
#' reticulate. It is necessary to have this installed in order to use any of the
#' functionality in the fawkes package.
#'
#' @param path The path to the AxiDraw API. If NULL it will be downloaded
#' automatically.
#' @inheritDotParams reticulate::py_install -packages
#'
#' @export
install_axidraw <- function(path = NULL, ...) {
  if (is.null(path)) {
    dir <- tempfile('pyaxidraw')
    archive <- tempfile('pyaxidraw.zip')
    dir.create(dir, recursive = TRUE)
    utils::download.file('https://cdn.evilmadscientist.com/dl/ad/public/AxiDraw_API.zip', archive)
    utils::unzip(archive, exdir = dir)
    path <- as.character(fs::dir_ls(dir))[1]
    unlink(archive)
    on.exit(unlink(dir, recursive = TRUE))
  }
  reticulate::py_install(path, ...)
  for (dep in list.files(file.path(path, "prebuilt_dependencies"), pattern = ".whl", full.names = TRUE)) {
    reticulate::py_install(dep)
  }
  invisible(NULL)
}

import_axidraw <- function() {
  axidraw <- try(reticulate::import('pyaxidraw.axidraw'))
  if (inherits(axidraw, 'try-error')) {
    rlang::abort('pyaxidraw could not be loaded.\nMake sure you have installed the library using `install_axidraw()`')
  }
  ad <- try(axidraw$AxiDraw())
  if (inherits(ad, 'try-error')) {
    rlang::abort('AxiDraw class could not be initiated.\nMake sure your AxiDraw is connected.')
  }
  ad
}

paper_dimensions <- function(name, portrait = TRUE) {
  if (is.numeric(name)) {
    if (!length(name) == 2) {
      rlang::abort('paper_size must be a numeric vector with width and height in mm')
    }
    if (any(name <= 0)) {
      rlang::abort("Dimensions must be positive")
    }
    size <- name
  } else {
    if (!is.character(name) || length(name) != 1) {
      rlang::abort('paper_size must be a character string')
    }
    size <- paper_formats[[tolower(name)]]
    if (is.null(size)) {
      rlang::abort(paste('Unknown paper format:', name))
    }
  }
  if ((portrait && size[2] < size[1]) ||
      (!portrait && size[1] < size[2])) {
    size <- rev(size)
  }
  size
}

paper_formats <- list(
  a0 = c(841, 1189),
  a1 = c(594, 841),
  a2 = c(420, 594),
  a3 = c(297, 420),
  a4 = c(210, 297),
  a5 = c(148, 210),
  a6 = c(105, 148),
  a7 = c(74, 105),
  a8 = c(52, 74),
  a9 = c(37, 52),
  a10 = c(26, 37),
  b0 = c(1000, 1414),
  b1 = c(707, 1000),
  b2 = c(500, 707),
  b3 = c(353, 500),
  b4 = c(250, 353),
  b5 = c(176, 250),
  b6 = c(125, 176),
  b7 = c(88, 125),
  b8 = c(62, 88),
  b9 = c(44, 62),
  b10 = c(31, 44),
  c0 = c(917, 1297),
  c1 = c(648, 917),
  c2 = c(458, 648),
  c3 = c(324, 458),
  c4 = c(229, 324),
  c5 = c(162, 229),
  c6 = c(114, 162),
  c7 = c(81, 114),
  c8 = c(57, 81),
  c9 = c(40, 57),
  c10 = c(28, 40),
  d1 = c(545, 771),
  d2 = c(385, 545),
  d3 = c(272, 385),
  d4 = c(192, 272),
  d5 = c(136, 192),
  d6 = c(96, 136),
  d7 = c(68, 96),
  e3 = c(400, 560),
  e4 = c(280, 400),
  e5 = c(200, 280),
  e6 = c(140, 200),
  quarto = c(254, 203.2),
  foolscap = c(330.2, 203.2),
  letter = c(215.9, 279.4),
  legal = c(215.9, 355.6),
  ledger = c(279.4, 431.8),
  tabloid = c(279.4, 431.8),
  executive = c(184.15, 266.7),
  post = c(393.7, 488.95),
  crown = c(381, 508),
  'large post' = c(419.1, 533.4),
  demy = c(444.5, 571.5),
  medium = c(457.2, 584.2),
  royal = c(508, 635),
  elephant = c(584.2, 711.2),
  'double demy' = c(596.9, 889),
  'quad demy' = c(889, 1143),
  a = c(215.9, 279.4),
  b = c(279.4, 431.8),
  c = c(431.8, 558.8),
  d = c(558.8, 863.6),
  e = c(863.6, 1117.6)
)

expand_margins <- function(margins) {
  switch(length(margins),
    rep_len(margins, 4),
    rep(margins, 2),
    margins[c(1, 2, 3, 2)],
    margins
  )
}
last <- function(x) x[[length(x)]]
`last<-` <- function(x, value) {
  x[[length(x)]] <- value
  x
}
first <- function(x) x[[1]]
`first<-` <- function(x, value) {
  x[[1]] <- value
  x
}

asym_dist <- function(start, end) {
  .Call("asym_dist_c", as.numeric(start), as.numeric(end), PACKAGE = 'fawkes')
}

apply_linetype <- function(x, y, lty, lwd) {
  .Call("line_pattern_c", as.numeric(x), as.numeric(y), as.integer(lty), as.numeric(lwd), PACKAGE = 'fawkes')
}
