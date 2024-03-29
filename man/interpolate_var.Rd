% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FVCOMPhysics.R
\name{interpolate_var}
\alias{interpolate_var}
\title{Given a set of points within the mesh, interpolate a variable such as bathymetry.}
\usage{
interpolate_var(p, X, var = "h")
}
\arguments{
\item{p}{sf POINT object}

\item{X}{FVCOMPhysics object with an element mesh}

\item{var}{character the variable to interpolate (bathymetry is h)}
}
\value{
numeric vector of interpolates, NA where points fall outside of the mesh
}
\description{
Note that some variables are colocated with nodes, while others are associated
with elements (triangular faces).  The former require interpolation, the later requires
more prep but no interpolation. The first dimension of any requested variable must
be 'node' (implemented) or "nele" (not implemented).  Any other is an error.
}
\details{
Nodes: We could just find the mean var of the three nodes surrounding each element,
but that assumes the point in in the centroid of the element which is likely to 
not be the case.  So, this uses simple plane geometry to interpolate the depth
at an arbitrary location within the mesh.
}
