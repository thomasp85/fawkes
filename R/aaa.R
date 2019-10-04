import_axidraw <- function() {
  axidraw <- try(reticulate::import('pyaxidraw'))
  if (inherits(axidraw, 'try-error')) {
    rlang::abort('pyaxidraw could not be loaded. Make sure you have installed the library')
  }
  axidraw$AxiDraw()
}
