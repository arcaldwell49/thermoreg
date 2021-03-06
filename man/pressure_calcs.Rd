% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/press.R
\name{press_init}
\alias{press_init}
\alias{avp_calc}
\alias{svp_calc}
\title{Pressure Calculations}
\usage{
press_init(
  est = NULL,
  type = "barometric",
  units = thermoreg_options("press_scale")
)

avp_calc(
  tdp = NULL,
  tdb = NULL,
  rh = NULL,
  temp = thermoreg_options("temp_scale"),
  units = thermoreg_options("press_scale")
)

svp_calc(
  tdb = NULL,
  temp = thermoreg_options("temp_scale"),
  units = thermoreg_options("press_scale")
)
}
\arguments{
\item{est}{a numeric value representing the measurement of interest.}

\item{type}{the type of pressure calculation.}

\item{units}{the units of measurement. Options include torr, atm, mbar (millibar), and psi. Default is torr.}

\item{tdp}{Dew point temperature}

\item{tdb}{Dry bulb temperature}

\item{rh}{Relative humidity in percentage units (e.g., 40 not .4)}

\item{temp}{the unit of measure for temperature. Default is celsius.}
}
\description{
Functions for calculating different forms of pressure
}
\section{Functions}{
\itemize{
\item \code{press_init}: Pressure measurement

\item \code{avp_calc}: Calculate actual vapor pressure

\item \code{svp_calc}: Calculate saturated vapor pressure
}}

