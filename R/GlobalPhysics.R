Global_Physics <-  R6::R6Class("Global_Physics",
                                
  inherit = FVCOM_Physics, 
  
  public = list(
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
GlobalPhysics <- function(filename = file.path("http://www.smast.umassd.edu:8080",
                                              "thredds/dodsC/models/fvcom/NECOFS/Forecasts",
                                              "NECOFS_FVCOM_OCEAN_GLOBAL_FORECAST.nc"),
                         ...){
  Global_Physics$new(filename, ...)
}