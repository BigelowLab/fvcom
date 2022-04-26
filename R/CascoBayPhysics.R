#' A class for navigating  FVCOM datasets
#' 
#' @description R6 base class for Boston Innundation 
#' @export
CascoBay_Physics <-  R6::R6Class("CascoBay",
                                
  inherit = FVCOM_Physics,
  
  #' @param filename character either a filename or OpenDAP URL
  #' @param origin POSIXct timestamp indicating the start of the experiment
  #' @param ... arguments for FVCOM_physics
  public = list(
    initialize = function(filename, 
                          origin = as.POSIXct("2018-05-01T00:00:00", tz = 'UTC'), 
                          ...){
      super$initialize(filename,
                       origin = origin,
                       ...)
    }
  )
)

#' Instantiate a FVCOM_Physics R6 object
#' 
#' @export
#' @param filename character either a filename or OpenDAP URL
#' @param ... other arguments passed to \code{FVCOM_Physics$new(filename, ...)}
#' @return FVCOM_Physics R6 Reference Object
CascoBayPhysics <- function(filename = "/mnt/ecocast/coredata/cascobay/cas3_May2018.nc",
                         ...){
  CascoBay_Physics$new(filename, ...)
}