MassBay_Physics <-  R6::R6Class("MassBay_Physics",
                                
  inherit = FVCOM_Physics, 
  
  public = list(
    initialize = function(...){
      super$initialize(...)
      self$zvar = "siglev"
    }
  )
)

#' Instantiate a FVCOM_Physics R6 object
#' 
#' @export
#' @param filename character either a filename or OpenDAP URL
#' @param ... other arguments passed to \code{FVCOM_Physics$new(filename, ...)}
#' @return FVCOM_Physics R6 Reference Object
MassBayPhysics <- function(filename = file.path("http://www.smast.umassd.edu:8080",
                                              "thredds/dodsC/models/fvcom/NECOFS/Archive",
                                              "NECOFS_MASS_BAY/2020/mbn_202012.nc"),
                         ...){
  MassBay_Physics$new(filename, ...)
}