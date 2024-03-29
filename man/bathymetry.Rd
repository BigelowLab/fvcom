% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{bathymetry}
\alias{bathymetry}
\title{Retrieve bathymetry "sea_floor_depth_below_geoid" in meters
with positive downward.}
\usage{
bathymetry(
  x,
  what = c("lonlat", "xy")[1],
  form = c("table", "raster", "mesh")[1]
)
}
\arguments{
\item{x}{ncdf4 object}

\item{what}{character either 'lonlat' or 'xy'}

\item{form}{character specifies output form "table", "raster" or "mesh"}
}
\value{
something
}
\description{
Retrieve bathymetry "sea_floor_depth_below_geoid" in meters
with positive downward.
}
