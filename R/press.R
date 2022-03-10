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

  if(units == "c"){
    units = "celsius"
  }
  if(units == "f"){
    units = "fahrenheit"
  }
  if(units == "k"){
    units = "kelvin"
  }

  if(!(units %in% c("celsius","fahrenheit","kelvin"))){
    stop("units not supported. Must be celsius, fahrenheit, and kelvin")
  }

  if(!(type %in% c("tdb","tdp","wbgt","tg","twb"))){
    stop("type not supported. Must be tdb, tdp, wbgt, tg, & twb")
  }

  structure(list(est = est,
                 type = type,
                 meas = "Temperature",
                 units = units),
            class = "thermoreg_meas")

}
