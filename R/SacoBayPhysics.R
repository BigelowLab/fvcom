#' A class for navigating FVCOM datasets
#' 
#' @description R6 base class for Saco Bay 
#' @export
SacoBay_Physics <-  R6::R6Class("SacoBay_Physics",
                                
  inherit = FVCOM_Physics, 
  
  public = list(
    #' @param ... arguments for FVCOM_physics
    initialize = function(...){
      super$initialize(...)
    }
  )
)

#' Instantiate a FVCOM_Physics R6 object
#' 
#' @export
#' @param filename character either a filename or OpenDAP URL
#' @param ... other arguments passed to \code{FVCOM_Physics$new(filename, ...)}
#' @return FVCOM_Physics R6 Reference Object
SacoBayPhysics <- function(filename = file.path("http://www.smast.umassd.edu:8080",
                                              "thredds/dodsC/models/fvcom/NECOFS/Forecasts",
                                              "NECOFS_FVCOM_OCEAN_SACO_FORECAST.nc"),
                         ...){
  SacoBay_Physics$new(filename, ...)
}