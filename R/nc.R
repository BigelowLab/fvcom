#' Test is an object is of ncdf4 class
#'
#' @export
#' @param x object to be tested
#' @return logical TRUE if the object is a valid ncdf4 class
is_ncdf4 <- function(x){
  inherits(x, "ncdf4")
}


#' Get the time dimension values.
#'
#' Note the the models are produced hourly, so we assume on the hour.  To get the
#' internally recorded hour set internal argument to TRUE.
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param origin POSIXct start date
#' @param internal logical, if TRUE use the internal times, otherwise assume on the hour
#' @return vector of POSIXct times
fvcom_time <- function(x,
                        origin = as.POSIXct("1858-11-17 00:00:00", tz = "UTC"),
                        internal = FALSE){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  if (internal){
    tv <- origin + (x$dim$time$vals * 86400)
  } else {
    t0 <- origin + (x$dim$time$vals[1] * 86400)
    tv <- seq(from = t0, by = 3600, length = length(x$dim$time$vals))
  }
  tv
}

#' Retrieve a count of nodes or elements
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param what character, either 'nodes'( or 'node') or 'elems' (or 'elem')
#' @return  number of nodes or elements
fvcom_count <- function(x, what = c("nodes", "elems")[1]){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  switch(tolower(what[1]),
         'node' = x$dim$node$len,
         'nodes' = x$dim$node$len,
         x$dim$nele$len)
}

#' Retrieve node locations
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param what character, either 'lonlat' (degrees) or 'xy' (meters)
#' @param index numeric, a 1-based index of desired nodes
#' @return data frame (tibble) of node locations
fvcom_nodes <- function(x,
                        what = c("lonlat", "xy")[1],
                        index = seq_len(fvcom_count(x, what = 'node'))){
  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  switch(tolower(what[1]),
               'lonlat'= dplyr::tibble(lon = ncdf4::ncvar_get(x, varid = "lon"),
                                       lat = ncdf4::ncvar_get(x, varid = "lat")),
               'xy'    = dplyr::tibble(x1 = ncdf4::ncvar_get(x, varid = "x"),
                                       y = ncdf4::ncvar_get(x, varid = "y")) %>%
                         dplyr::rename(x = x1) ) %>%
      dplyr::slice(index) %>%
    tibble::add_column(node = index, .before = 1)
}

#' Retrieve element locations
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param what character, either 'lonlat' (degrees) or 'xy' (meters)
#' @param index numeric, a 1-based index of desired nodes
#' @return data frame (tibble) of node locations
fvcom_elems <- function(x,
                        what = c("lonlat", "xy")[1],
                        index = seq_len(fvcom_count(x, what = 'elems'))){
  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  switch(tolower(what[1]),
         'lonlat'= dplyr::tibble(lon = ncdf4::ncvar_get(x, varid = "lonc"),
                                 lat = ncdf4::ncvar_get(x, varid = "latc")),
         'xy'    = dplyr::tibble(x1 = ncdf4::ncvar_get(x, varid = "xc"),
                                 y = ncdf4::ncvar_get(x, varid = "yc")) %>%
                   dplyr::rename(x = x1) ) %>%
    dplyr::slice(index) %>%
    tibble::add_column(elem = index, .before = 1)
}


#' Retrieve the index closest to the specified value(s) for 1-d dimensions.
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param vals numeric or POSIXct, one or more values to convert to indices
#' @param what character, one of the objects one-dimension names
#' @return vector of indices closest to the specifed val - one for each val
fvcom_index1 <- function(x, vals = 0, what = c('time')[1]){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")
  allowed <- c("time")
  ix <- what[1] %in% allowed
  if (!ix) stop("requested dim must be one of:", paste(allowed, collapse = " "))

  ix <- what[1] %in% names(x$dim)
  if (!ix) stop("requested dim not found in resource:", what[1])

  v = x$dim[[what[1]]]$vals
  sapply(vals,
         function(val, lut = NULL){
           which.min(abs(lut - val))
         })
}


#' Retrieve the navigation elements, start and count, for extracting slabs of data.
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param vals two element numeric or POSIXct, start and stop values
#' @param what character, one of the objects dimension names
#' @param indexed logical, if TRUE then siglay and time are 1-based indices,
#'        but if FALSE then siglay and time are actual values to be located.
#' @return two element vector of start and count
fvcom_nav <- function(x,
                      vals = c(0, 1),
                      what = c('siglay', 'time', 'siglev')[1],
                      indexed = FALSE){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")
  if (!indexed){
      vals <- fvcom_index(x, vals, what = what)
  }
  vals = sort(vals)
  c(start = vals[1], count = vals[2] - vals[1])
}

