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
    stop("units not supported. Must be celsius, fahrenheit, and kelvin.")
  }

  if(!(type %in% c("tdb","tdp","wbgt","tg","twb"))){
    stop("type not supported. Must be tdb, tdp, wbgt, tg, & twb.")
  }

  structure(list(est = est,
                 type = type,
                 meas = "Temperature",
                 units = units),
            class = "thermoreg_meas")

}


#' @describeIn temperature_calcs Dew point calculation
#' @export

tdp_calc = function(tdb = NULL,
                    rh = NULL,
                    units = thermoreg_options("temp_scale")){
  if(class(tdb) == "thermreg_meas"){
    if(tdb$units != units){
      stop("input temperature units from thermoreg_meas must match units argument")
    }
  } else if(class(tdb) == "numeric"){
    tdb = temp_init(est = tdb,
                    type = "tdb",
                    units = units)
  } else {
    stop("tdp input must be thermoreg_meas or numeric")
  }

  x = temp_convert(x = tdb,
                   to = "celsius")
  # Magnus-Tetens formula
  a = 17.62
  b = 243.12
  alpha_func = log(rh/100) + a*x$est/(b+x$est)
  t2 = (b*alpha_func / (a - alpha_func))
  t3 = temp_convert(x = t2,
                    from = "celsius",
                    to = units)
  structure(list(est = t3$est,
                 type = "tdp",
                 meas = "Temperature",
                 units = t3$units),
            class = "thermoreg_meas")
}

