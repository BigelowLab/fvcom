% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track.R
\name{track_add_distance}
\alias{track_add_distance}
\title{Append the distance between points along a one or more tracks in a data frame}
\usage{
track_add_distance(
  x,
  by_element = TRUE,
  as_vector = TRUE,
  cumulative = TRUE,
  ...
)
}
\arguments{
\item{x}{sf POINT object, with a \code{track} column.  If the \code{track} column is not 
present one is added.}

\item{by_element}{logical, defaults to TRUE, see \code{\link[sf]{st_distance}}}

\item{as_vector}{logical, to return a numeric vector otherwise an \code{units}
object is returned}

\item{cumulative}{logical, if TRUE retrieve the cumulative distance.  Ignored
if \code{as_vector = FALSE}}

\item{...}{other arguments for \code{\link[sf]{st_distance}}}
}
\value{
the input sf POINT object with added \code{path_distance} variable (column)
}
\description{
Append the distance between points along a one or more tracks in a data frame
}
