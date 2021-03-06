% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-thermoreg_meas.R
\name{measure-methods}
\alias{measure-methods}
\alias{print.thermoreg_meas}
\title{Methods for thermoreg_meas objects}
\usage{
\method{print}{thermoreg_meas}(x, ...)
}
\arguments{
\item{x}{object of class \code{thermoreg_meas} as returned from a function starting ending in "calc"}

\item{...}{further arguments passed through}
}
\value{
The print method will print the measurement type (e.g., Temperature), the estimated value (to 4 significant digits), and the units in which it is measured (e.g., celsius).
}
\description{
Methods defined for objects returned from calc functions in the thermoreg package.
}
