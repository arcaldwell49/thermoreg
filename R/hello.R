# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

avp = function(dp = NULL,
               ta = NULL,
               rh = NULL,
               temp = "celsius"){

  if(!is.null(dp)){
    avp = 6.11 * 10^((7.5 * dp)/(237.3 + dp))
  } else {

  }

  # 0.750062 hPa/mbar -> mmHg
}

svp = function(ta = NULL){
  return()
}

temp_convert = function(t = NULL,
                        to = "celsius",
                        from = "fahrenheit"){
  if(to == "celsius"){
    if(from == "fahrenheit"){
      t2 = 5/9*(t-32)
    } else if(from == "kelvin"){
      t2 = t2 - 273
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
      t2 = t2 + 273
    } else{
      t2 = t
    }
  }

  return(t2)
}
