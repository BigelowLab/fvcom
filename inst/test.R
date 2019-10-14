

library(sf)
library(fvcom)
library(ncdf4)
library(tidyverse)
library(leaflet)

setwd("/mnt/ecocast/corecode/R/fvcom")
uri <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/NECOFS_GOM/2019//gom4_201901.nc"
x <- nc_open(uri)

what = 'lonlat'
where = 'elems'

#npts = fvcom_nodes(x, form = 'sf', what = 'xy')
epts = fvcom::fvcom_elems(x, form = 'sf', what = what)
mesh <- emesh <- fvcom::get_mesh(x, vars = c("u", "v"), what = what)
#nmesh = get_node_mesh_geometry(x, what = 'lonlat')

leaflet(mesh) %>%
  addTiles() %>%
  addPolygons(fill = TRUE, weight = 1, color = "#FF0000")

