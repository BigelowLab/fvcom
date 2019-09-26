library(sf)
library(fvcom)
library(ncdf4)
library(tidyverse)
setwd("/mnt/ecocast/corecode/R/fvcom")
mgrepl <- function(pattern, x, op = `|`, ... ){
  Reduce(op, lapply(pattern, grepl, x, ...))
}

if (FALSE) {
  uri <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/NECOFS_GOM/2019//gom4_201901.nc"
  x <- nc_open(uri)
}

#getting node network
if(FALSE){
  index <- seq_len(fvcom_count(x,"nodes"))
  form   <- 'sf'
  ntsn   <- ncdf4::ncvar_get(x, "ntsn")[index]
  nbsn <- ncdf4::ncvar_get(x, "nbsn")[index,]
  all_nodes <- fvcom_nodes(x, form = form)
  get_nidx <- function(index, ntsn, nbsn){
    nbsn[index, 1:ntsn[index]]
  }
  nav <- cbind(nbsn, ntsn)
  n <- ncol(nav)
  all_nodes <- all_nodes %>%
    tibble::add_column(nbs = apply(nav, 1, function(x) x[1:(x[n])] ), .after = "node")
}
