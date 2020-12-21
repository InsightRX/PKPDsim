.onUnload <- function(libpath) {
  library.dynam.unload(utils::packageName(), libpath)
}
