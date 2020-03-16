

library(sf)
library(fvcom)
library(ncdf4)
library(tidyverse)
library(leaflet)



setwd("/mnt/ecocast/corecode/R/necofs")
uri <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/NECOFS_GOM/2019//gom4_201901.nc"
x <- nc_open(uri)

what = 'lonlat'
where = 'elems'

epts = fvcom::fvcom_elems(x, form = 'sf', what = what)
mesh <- mesh_ll <- fvcom::get_mesh(x, vars = "h",
                                   mesh = get_elem_mesh_geometry(x, what = "lonlat")) %>%
  sf::write_sf(dsn = "/mnt/ecocast/coredata/necofs/mesh/fvcom_v3_lonlat.geojson")
mesh_xy <- fvcom::get_mesh(x, vars = "h",
                           mesh = get_elem_mesh_geometry(x, what = "xy")) %>%
  sf::write_sf(dsn = "/mnt/ecocast/coredata/necofs/mesh/fvcom_v3_xy.geojson")
