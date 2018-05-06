.onLoad <- function(libname, pkgname) {
  inspect <<- reticulate::import("inspect", delay_load = TRUE)
}
