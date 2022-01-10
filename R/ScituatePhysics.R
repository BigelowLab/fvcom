Scituate_Physics <-  R6::R6Class("Scituate_Physics",
                                
  inherit = FVCOM_Physics, 
  
  public = list(
    initialize = function(...){
      super$initialize(...)
      self$zvar = "siglay"
    }
  )
)

#' Instantiate a FVCOM_Physics R6 object
#' 
#' @export
#' @param filename character either a filename or OpenDAP URL
#' @param ... other arguments passed to \code{FVCOM_Physics$new(filename, ...)}
#' @return FVCOM_Physics R6 Reference Object
ScituatePhysics <- function(filename = file.path("http://www.smast.umassd.edu:8080",
                                              "thredds/dodsC/models/fvcom/NECOFS/Forecasts",
                                              "NECOFS_FVCOM_OCEAN_SCITUATE_FORECAST.nc"),
                         ...){
  Scituate_Physics$new(filename, ...)
}