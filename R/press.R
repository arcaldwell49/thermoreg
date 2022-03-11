#' Pressure Calculations
#'
#' Functions for calculating different forms of pressure
#'
#' @param est a numeric value representing the measurement of interest.
#' @param type the type of pressure calculation.
#' @param units the units of measurement. Options include torr, atm, mbar (millibar), and psi. Default is torr.
#' @param temp the unit of measure for temperatuer. Default is celsius.
#' @param tdp Dew point temperature
#' @param tdb Dry bulb temperature
#' @param rh Relative humiditiy in percentage units (e.g., 40 not .4)
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

#' @describeIn pressure_calcs Calculate actual vapor pressure
#' @export
avp_calc = function(tdp = NULL,
                    tdb = NULL,
                    rh = NULL,
                    temp = thermoreg_options("temp_scale"),
                    units = thermoreg_options("press_scale")){

  if(class(tdp) == "thermoreg_meas"){
    tdp = tdp$est
  }

  if(class(tdb) == "thermoreg_meas"){
    tdb = tdb$est
  }

  if(class(rh) == "thermoreg_meas"){
    rh = rh$est
  }

  if(temp != "celsisus" | temp != "c"){
    if(!is.null(tdp)){
      tdp = temp_convert(x = tdp,
                         to = "celsius",
                         from = temp,
                         type = "tdp")$est
    }

    if(!is.null(tdb)){
      tdp = temp_convert(x = tdp,
                         to = "celsius",
                         from = temp,
                         type = "tdb")$est
    }
  }

  if(!is.null(tdp)){
    e_a = 6.11 * 10^((7.5 * tdp)/(237.3 + tdp))
  } else {
    e_s = svp_calc(tdb,
                   units = "mbar")$est
    e_a = rh/100*e_s

  }

  e_a = press_convert(e_a,
                      from = "mbar",
                      to = units)$est

  structure(list(est = e_a,
                 type = "avp",
                 meas = "Pressure",
                 units = units),
            class = "thermoreg_meas")
  # 0.750062*mbar/mbarar -> mmHg
  # 1/5171493256*mmHg -> psi
  # 1/760
}

#' @describeIn pressure_calcs Calculate saturated vapor pressure
#' @export
svp_calc = function(tdb = NULL,
                    temp = thermoreg_options("temp_scale"),
                    units = thermoreg_options("press_scale")){

  if(class(tdb) == "thermoreg_meas"){
    tdb = tdb$est
  }
  if(temp != "celsisus" | temp != "c"){
    if(!is.null(tdp)){
      tdp = temp_convert(x = tdp,
                         to = "celsius",
                         from = temp,
                         type = "tdp")$est
    }
  }
  e_s = (6.11 * 10^((7.5 * tdb)/(237.3 + tdb)))

  e_s = press_convert(e_s,
                      to = units,
                      from = "mbar")$est

  structure(list(est = e_s,
                 type = "svp",
                 meas = "Pressure",
                 units = units),
            class = "thermoreg_meas")
}



