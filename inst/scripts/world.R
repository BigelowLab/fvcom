library(fvcom)
library(necofs)
library(ncdf4)

uri <- 'http://www.smast.umassd.edu:8080/thredds/dodsC/FVCOM/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_GLOBAL_FORECAST.nc'

X = nc_open(uri)
nc_close(X)