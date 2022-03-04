suppressPackageStartupMessages({
  library(ncdf4)
  library(readr)
  library(dplyr)
  #library(fvcom)
  library(sf)
})


devtools::load_all("/mnt/ecocast/corecode/R/fvcom")

read_start <- function(filename = "ROPOS2019.geojson",
                       crs = NULL){
  x <- sf::read_sf(filename)
  if (!is.null(crs)) x <- sf::st_transform(x, crs)
  x
}

BASE_URL <- 'http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/NECOFS_GOM'
URL <- file.path(BASE_URL,'2019/gom4_201906.nc')
X <- fvcom::FVCOMPhysics(URL)

P0 <- read_start("/mnt/ecocast/projects/fvcom/ROPOS2019/ROPOS2019.geojson",
                 crs = sf::st_crs(X$M)) |>
  dplyr::slice(5)

tstep = 60 * 15
tmax = 60 * 60 * 24 * 30
reverse = TRUE
verbose = FALSE
show_progress = FALSE
drag = c(0,0,0)
fixed_z = TRUE
clip_z = TRUE
