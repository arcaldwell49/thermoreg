
avp_calc = function(tdp = NULL,
                    tdb = NULL,
                    rh = NULL,
                    temp = "celsius",
                    units = "torr"){

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
    e_s = svp_calc(tdb)$est
    e_a = rh/100*e_s

  }

  structure(list(est = e_a,
                 type = "avp",
                 meas = "Pressure",
                 units = "mb"),
            class = "thermoreg_meas")
  # 0.750062*mb/mbar -> mmHg
  # 1/5171493256*mmHg -> psi
  # 1/760
}

svp_calc = function(tdb = NULL,
                    temp = "celsius",
                    units = "torr"){

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
                      from = "mb")$est

  structure(list(est = e_s,
                 type = "svp",
                 meas = "Pressure",
                 units = units),
            class = "thermoreg_meas")
}


