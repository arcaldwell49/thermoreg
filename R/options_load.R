## set default options for thermoreg_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.thermoreg <- list(
    thermoreg.temp_scale = "celsius",
    thermoreg.press_scale = "torr",
  )
  toset <- !(names(op.thermoreg) %in% names(op))
  if (any(toset)) options(op.thermoreg[toset])

  invisible()
}
