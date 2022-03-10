#' Methods for thermoreg_meas objects
#'
#' Methods defined for objects returned from calc functions in the thermoreg package.
#'
#' @param x object of class \code{thermoreg_meas} as returned from a function starting ending in "calc"
#' @param ... further arguments passed through
#' @returns
#'
#' The print method will print the measurement type (e.g., Temperature), the estimated value (to 4 significant digits), and the units in which it is measured (e.g., celsius).
#' @name measure-methods


### methods for thermoreg_meas

#' @rdname measure-methods
#' @method print thermoreg_meas
#' @export

print.thermoreg_meas <- function(x,...){
  # est, meas, units
  est = x$est
  meas = x$meas
  units = x$units
  type = x$type
  cat(meas,  sep = "")
  cat("\n")
  cat(type, "=",signif(est,4), units)

}
