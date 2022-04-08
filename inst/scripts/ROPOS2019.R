suppressPackageStartupMessages({
  library(ncdf4)
  library(readr)
  library(dplyr)
  library(fvcom)
  library(sf)
})


#devtools::load_all("/mnt/ecocast/corecode/R/fvcom")

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
                 crs = "+init=nad83:1802") 

tstep = 60 * 15
tmax = 60 * 60 * 24 * 30
reverse = TRUE
verbose = FALSE
show_progress = TRUE
drag = c(0,0,0)
fixed_z = TRUE
clip_z = TRUE

pp <- particle_track(X, P0 = P0,
                     tstep = tstep, # 15 minutes
                     tmax = tmax,  # 30 days
                     reverse = reverse,
                     verbose = verbose,
                     clip_z = clip_z,
                     show_progress = show_progress,
                     filename = "ROPOS2019-06-zclip-track.geojson")

X = 0
