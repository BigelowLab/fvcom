#title: GOM3 Nesting  by Yf.Sun@umassd.edu
#institution: School for Marine Science and Technology
#source: FVCOM_3.0
#history: model started at: 22/12/2011   16:41
#references: http://fvcom.smast.umassd.edu, http://codfish.smast.umassd.edu
library(sf)
library(fvcom)
library(ncdf4)
library(tidyverse)

uri <- "http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3"

x <- nc_open(uri)

what = 'lonlat'
where = 'elems'

epts = fvcom::fvcom_elems(x, form = 'sf', what = what)
mesh <- mesh_ll <- fvcom::get_mesh(x, vars = "h",
                                   mesh = get_elem_mesh_geometry(x, what = "lonlat"))
