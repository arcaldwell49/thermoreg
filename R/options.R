#' Set/get global thermoreg options
#'
#' Global thermoreg options, but can be changed in each functions directly using
#' an argument (which has precedence over the global options).
#'
#' @param ... One of four: (1) nothing, then returns all options as a list; (2)
#'   a name of an option element, then returns its' value; (3) a name-value pair
#'   which sets the corresponding option to the new value (and returns nothing),
#'   (4) a list with option-value pairs which sets all the corresponding
#'   arguments. The example show all possible cases.
#'
#' @details The following arguments are currently set:
#' \itemize{
#' \item \code{verbose} should verbose (printed results) be set to true? Default is \code{TRUE}.
#' }
#'
#' @note All options are saved in the global R \code{\link{options}} with prefix
#'   \code{thermoreg.}
#'
#' @return depends on input, see above.
#' @export


thermoreg_options <- function(...) {
  dots <- list(...)
  #browser()
  if (length(dots) == 0) {  # branch to get all thermoreg options
    op <- options()
    thermoreg_op <- op[grepl("^thermoreg.", names(op))]
    names(thermoreg_op) <- sub("^thermoreg.", "", names(thermoreg_op))
    return(thermoreg_op)
  } else if (is.list(dots[[1]])) {  # set several thermoreg options as a list:
    newop <- dots[[1]]
    names(newop) <- paste0("thermoreg.", names(newop))
    options(newop)
  } else if (!is.null(names(dots))) {
    newop <- dots
    names(newop) <- paste0("thermoreg.", names(newop))
    options(newop)
  } else if (is.null(names(dots))) {  # get a single thermoreg options
    if (length(dots) > 1) stop("thermoreg_options() can only return the value of a single option.", call. = FALSE)
    return(getOption(paste0("thermoreg.", unlist(dots))))
  } else {
    warning("Unsupported command to thermoreg_options(), nothing done.", call. = FALSE)
  }
}
