#' Temperature Calculations
#'
#' Functions for calculating different forms of temperature.
#'
#' @param est a numeric value representing the measurement of interest.
#' @param type the type of temperature calculation.
#' @param units the units of measurement. Options include Celsius, Fahrenheit, or Kelvin. Default is celsius.
#'
#' @describeIn temperature_calcs Temperature measurement
#' @export
#'

temp_init = function(est = NULL,
                     type = "tdb",
                     units = thermoreg_options("temp_scale")){
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

