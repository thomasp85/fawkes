axi_manual <- function(options = list()) {
  axidraw <- import_axidraw()
  for (opt in names(options)) {
    axidraw$options[[opt]] <- options[[opt]]
  }
  axidraw$interactive()
  axidraw
}
