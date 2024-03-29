% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nc.R
\name{get_elem_var}
\alias{get_elem_var}
\title{Get variables for a set of elements.  This is a convenience
function that provides a means for getting a vector of values at a given
sigma layer (or level) and time.  Note that the dimensions a
variable uses varies - see \code{\link{list_vars}}.}
\usage{
get_elem_var(
  x,
  var = c("u", "v"),
  elem = seq_len(fvcom_count(x, "elems")),
  y = 1,
  time = 1,
  y_transform = FALSE
)
}
\arguments{
\item{x}{FVCOM ncdf4 object}

\item{var}{charcater the name of the variable.  Not all variables are handled
by this function.}

\item{elem}{integer one or more 1-based node ID(s).  These must form
a contiguous sequence of elems identifers}

\item{y}{integer a single 1-based index into siglay, or sigelev}

\item{time}{either a single 1-based integer or POSIXct}

\item{y_transform}{logical, if convert y from sigma to integer?
Note that time is auto-converted}
}
\value{
tibble of element id and values - one row for each node
}
\description{
Get variables for a set of elements.  This is a convenience
function that provides a means for getting a vector of values at a given
sigma layer (or level) and time.  Note that the dimensions a
variable uses varies - see \code{\link{list_vars}}.
}
