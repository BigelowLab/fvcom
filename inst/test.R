
if (FALSE) {
  library(sf)
  library(fvcom)
  library(ncdf4)
  library(tidyverse)
  setwd("/mnt/ecocast/corecode/R/fvcom")
  uri <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/NECOFS_GOM/2019//gom4_201901.nc"
  x <- nc_open(uri)
  emesh = get_elem_mesh_geometry(x)
  nmesh = get_node_mesh_geometry(x)
}
