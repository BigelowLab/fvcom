% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track.R
\name{read_track}
\alias{read_track}
\title{Read a track}
\usage{
read_track(filename, crs = c(NA, "+init=nad83:1802")[2])
}
\arguments{
\item{filename}{the name of the file to read}

\item{crs}{NA or crs-castable object to transform the data}
}
\value{
sf object
}
\description{
Read a track
}
