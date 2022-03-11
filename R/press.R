#' Pressure Calculations
#'
#' Functions for calculating different forms of pressure
#'
#' @param est a numeric value representing the measurement of interest.
#' @param type the type of pressure calculation.
#' @param units the units of measurement. Options include torr, atm, mbar (millibar), and psi. Default is torr.
#'
#' @describeIn pressure_calcs Pressure measurement
#' @export
#'

press_init = function(est = NULL,
                      type = "barometric",
                      units = thermoreg_options("press_scale")){
  type = tolower(type)
  units = tolower(units)
  est = as.numeric(est)

  if(!(units %in% c("mbar","torr","psi","atm"))){
    stop("units not supported. Must be mbar, torr, psi, or atm")
  }

  if(!(type %in% c("barometric", "avp", "svp"))){
    stop("type not supported. Must be barometric, avp, or svp")
  }

  structure(list(est = est,
                 type = type,
                 meas = "Pressure",
                 units = units),
            class = "thermoreg_meas")

}
