axi_svg <- function(file, options = list(), capture = FALSE) {
  file <- fs::path_expand(file)
  axidraw <- import_axidraw()
  axidraw$plot_setup(file)
  for (opt in names(options)) {
    axidraw$options[[opt]] <- options[[opt]]
  }
  axidraw$plot_run(capture)
}
