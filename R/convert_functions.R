#' Unit Conversion Functions
#'
#' Functions for providing conversions between scales of measurement.
#'
#' @param x a numeric value representing the measurement of interest
#' @param to the scale to convert to
#' @param from the scale to convert from
#'
#' @describeIn convert_functions Conversion for Temperature
#' @export

temp_convert = function(x = NULL,
                        to = thermoreg_options("temp_scale"),
                        from = "fahrenheit",
                        type = "tdb"){
  if(class(x) == "thermoreg_meas"){
    x = x$est
  }
  x = as.numeric(x)
  to = tolower(to)
  from = tolower(from)
  if(!(to %in% c("kelvin", "celsius", "fahrenheit", "c", "f","k"))){
    stop("to: Not a supported measure of temperature")
  }

  if(!(from %in% c("kelvin", "celsius", "fahrenheit", "c", "f","k"))){
    stop("from: Not a supported measure of temperature")
  }

  t = x
  if(to %in% c("c","f","k")){
    if(to == "c"){
      to = "celsius"
    } else if ( to == "f"){
      to = "fahrenheit"
    } else if ( to == "k"){
      to = "kelvin"
    }
  }

  if(from %in% c("c","f","k")){
    if(from == "c"){
      from = "celsius"
    } else if ( from == "f"){
      from = "fahrenheit"
    } else if ( from == "k"){
      from = "kelvin"
    }
  }

  if(to == "celsius"){
    if(from == "fahrenheit"){
      t2 = 5/9*(t-32)
    } else if(from == "kelvin"){
      t2 = t - 273
    } else{
      t2 = t
    }

  }

  if(to == "fahrenheit"){
    if(from == "celsius"){
      t2 = 9/5*(t) + 32
    } else if(from == "kelvin"){
      t2 = 9/5*(t-273)+32
    } else{
      t2 = t
    }
  }

  if(to == "kelvin"){
    if(from == "fahrenheit"){
      t2 = 5/9*(t-32)+273
    } else if(from == "celsius"){
      t2 = t + 273
    } else{
      t2 = t
    }
  }

  structure(list(est = t2,
                 type = type,
                 meas = "Temperature",
                 units = to),
            class = "thermoreg_meas")
}

#' @describeIn convert_functions Conversion for Pressure
#' @export

press_convert = function(x = NULL,
                         to = thermoreg_options("press_scale"),
                         from = "mbar",
                         type = "barometric"){

  to = tolower(to)
  from = tolower(from)

  if(class(x) == "thermoreg_meas"){
    x = x$est
  }

  if(!(to %in% c("torr","mmhg","psi","mbar","atm"))){
    stop("to: Not a supported measure of pressure")
  }

  if(!(from %in% c("torr","mmhg","psi","mbar","atm"))){
    stop("from: Not a supported measure of pressure")
  }

  if(to == "mmhg"){
    to == "torr"
  }

  if(from == "mmhg"){
    from == "torr"
  }
  # 0.750062*mbar/mbarar -> mmHg
  # 1/51.71493256*mmHg -> psi
  # 1/760

  # convert everything to torr
  if(from == "mbar"){
    p1 = 0.750062*x
  }else if(from == "psi"){
    p1 = 51.7149*x
  } else if (from == "atm"){
    p1 = 760*x
  } else {
    p1 = x
  }

  if(to == "torr"){
      p2 = p1
  }

  if(to == "atm"){
      p2 = p1/760
  }

  if(to == "mbar"){
    p2 = 1/0.750062*p1
  }

  if(to == "psi"){
  p2 = 1/51.7149*p1

  }

  structure(list(est = p2,
                 type = type,
                 meas = "Pressure",
                 units = to),
            class = "thermoreg_meas")

}
