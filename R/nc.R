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
#' @param where character, either 'nodes'( or 'node') or 'elems' (or 'elem')
#' @return  number of nodes or elements
fvcom_count <- function(x, where = c("nodes", "elems")[1]){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  switch(tolower(where[1]),
         'node' = x$dim$node$len,
         'nodes' = x$dim$node$len,
         x$dim$nele$len)
}

#' Retrieve node locations
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param what character, either 'lonlat' (degrees) or 'xy' (meters)
#' @param index NULL or numeric, a 1-based index of desired nodes.  If NULL then
#'   all nodes are returned
#' @param form character either 'table' or 'sf'
#' @param include character or NULL items to add to the returned data. Multiple items
#'   can be included, but note that "none" is the same as NULL so nothing gets included.
#' \itemize{
#'   \item{none don't include any extra information  - same as setting to NULL}
#'   \item{nbs include a list of the indices of the neighbors}
#'   \item{art1 Area of elements around a node}
#'   \item{art2 Area of Node-Base Control volume}
#' }
#' @return data frame (tibble) of node locations including...
#' \itemize{
#'   \item{node node index identifier}
#'   \item{lon (or x) locations value}
#'   \item{lat (or y) location value}
#' }
#' Or a sf collection of type POINT
fvcom_nodes <- function(x,
                        what = c("lonlat", "xy")[1],
                        index = NULL,
                        form = c("table", "sf")[1],
                        include = NULL){
  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")
  idx <- seq_len(fvcom_count(x, "nodes"))
  r <- switch(tolower(what[1]),
               'lonlat'= dplyr::tibble(node = idx,
                                       lon = ncdf4::ncvar_get(x, varid = "lon"),
                                       lat = ncdf4::ncvar_get(x, varid = "lat")),
               'xy'    = dplyr::tibble(node = idx,
                                       x1 = ncdf4::ncvar_get(x, varid = "x"),
                                       y = ncdf4::ncvar_get(x, varid = "y")) %>%
                         dplyr::rename(x = .data$x1) )

  if (inherits(include, "character")){
    include <- tolower(include)
    if ("art2" %in% include){
      r <- r %>%
        tibble::add_column(art2 = ncdf4::ncvar_get(x, varid = "art2"), .after = "node")
    }
    if ("art1" %in% include){
      r <- r %>%
        tibble::add_column(art1 = ncdf4::ncvar_get(x, varid = "art1"), .after = "node")
    }
    if ("nbs" %in% include){
      ntsn   <- ncdf4::ncvar_get(x, "ntsn")
      nbsn <- ncdf4::ncvar_get(x, "nbsn")
      nav <- cbind(nbsn, ntsn)
      n <- ncol(nav)
      r <- r %>%
        tibble::add_column(nbs = apply(nav, 1, function(x) x[1:(x[n])] ), .after = "node")
    }
  }

   if (!is.null(index)){
     r <- r %>%
       dplyr::slice(index)
   }

   if (tolower(form[1]) == 'sf'){
     coords <- switch(tolower(what),
                      "xy" = c("x", "y"),
                      c("lon", "lat"))
     r <- r %>%
       sf::st_as_sf(coords = coords, crs = fvcom_crs(what))
   }

   r
}

#' Retrieve element locations
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param what character, either 'lonlat' (degrees) or 'xy' (meters)
#' @param index numeric, a 1-based index of desired nodes
#' @param form character either 'table' or 'sf'
#' @param include character or NULL items to add to the returned data. Multiple items
#'   can be included, but note that "none" is the same as NULL so nothing gets included.
#' \itemize{
#'   \item{none don't include any extra information  - same as setting to NULL}
#'   \item{nbs include a list of the indices of the neighbors}
#' }
#' @return data frame (tibble) of element locations including...
#' \itemize{
#'   \item{elem element index identifier}
#'   \item{lon (or x) locations value}
#'   \item{lat (or y) location value}
#' }
#' Or a sf collection of type POINT
fvcom_elems <- function(x,
                        what = c("lonlat", "xy")[1],
                        index = NULL,
                        form = c("table", "sf"),
                        include = NULL){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  idx <- seq_len(fvcom_count(x, where = 'elems'))
  r <- switch(tolower(what[1]),
         'lonlat'= dplyr::tibble(elem = idx,
                                 lon = ncdf4::ncvar_get(x, varid = "lonc"),
                                 lat = ncdf4::ncvar_get(x, varid = "latc")),
         'xy'    = dplyr::tibble(elem = idx,
                                 x1 = ncdf4::ncvar_get(x, varid = "xc"),
                                 y = ncdf4::ncvar_get(x, varid = "yc")) %>%
                   dplyr::rename(x = .data$x1) )

  if (inherits(include, "character")){
    include <- tolower(include)
    if ("nbs" %in% include){
      #int ntve[node]
      #long_name: #elems surrounding each node
      #  int nbve[node,maxelem]
      #long_name: elems surrounding each node
      ntve   <- ncdf4::ncvar_get(x, "ntve")
      nbve <- ncdf4::ncvar_get(x, "nbve")
      nav <- cbind(nbve, ntve)
      n <- ncol(nav)
      r <- r %>%
        tibble::add_column(nbs = apply(nav, 1, function(x) x[1:(x[n])] ), .after = "node")
    }
  }

   if (!is.null(index)){
     r <- r %>%
       dplyr::slice(index)
   }

   if (tolower(form[1]) == 'sf'){
     coords <- switch(tolower(what),
                      "xy" = c("x", "y"),
                      c("lon", "lat"))
     r <- r %>%
       sf::st_as_sf(coords = coords, crs = fvcom_crs(what))
   }
   r
}


#' Retrieve the index closest to the specified value(s) for 1-d dimensions.
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param vals numeric or POSIXct, one or more values to convert to indices
#' @param what character, one of the objects one-dimension names
#' @return vector of indices closest to the specifed val - one for each val
fvcom_index <- function(x, vals = 0, what = c('time')[1]){

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
         }, lut = v)
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
  if (indexed){
      vals <- fvcom_index(x, vals, what = what)
  }
  vals = sort(vals)
  c(start = vals[1], count = vals[2] - vals[1])
}


#' Retrieve the CRS
#'
#' @export
#' @param what character either 'xy', 'lonlat' or 'longlat'.  Actually, if not 'xy'
#'   then 'lonlat' is returned.
#' @return character CRS
fvcom_crs <- function(what = "lonlat"){
  crs <- switch(tolower(what[1]),
                "xy" = "+init=nad83:1802",
                       "+init=epsg:4326")
}

#' Get variables for nodes or elements.
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param where character - either 'node' or 'elem'
#' @param ... further arguments for \code{\link{get_node_var}} or \code{\link{get_elem_var}}
#' @return tibble - see \code{\link{get_node_var}} or \code{\link{get_elem_var}}
get_var <- function(x, where = c("nodes", "elems"), ...){
  switch(tolower(where[1]),
    'elems' = get_elem_var(x, ...),
              get_node_var(x, ...)
  )
}

#' Get variables for a set of nodes.  This is a convenience
#' function that provides a means for getting a vector of values at a given
#' sigma layer (or level) and time.  Note that the dimensions a
#' variable uses varies - see \code{\link{list_vars}}.
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param var charcater the name of the variable.  Not all variables are handled
#'   by this function.
#' @param node integer one or more 1-based node ID(s).  These must form
#'   a contiguous sequence of node identifers
#' @param y integer a single 1-based index into siglay, or sigelev
#' @param time either a single 1-based integer of POSIXct
#' @param y_transform logical, if convert y from sigma to integer?
#'    Note that time is auto-converted
#' @return tibble of node id and values - one row for each node
get_node_var <- function(x, var = 'zeta',
                    node = seq_len(fvcom_count(x, "nodes")),
                    y = 1,
                    time = 1,
                    y_transform = FALSE){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  if (length(var) > 1){
    vv <- lapply(var,
      function(v){
        get_node_var(x, var = v, node = node, y = y, time = time, y_transform = y_transform)
      })
    r <- Reduce(function(a,b) dplyr::left_join(a, b, by = "node"), vv)
    return(r)
  }

  v <- list_vars(x, collapse = FALSE) %>%
      dplyr::filter(.data$name == var[1])
  if (nrow(v) == 0) stop("variable not found:", var[1])
  if (inherits(time, 'POSIXct')) time <- time_index(x, time)
  if (y_transform) y <- sigma_index(x, y, what = v$name[1][2])

  start <- node[1]
  count <- length(node)
  if (length(v$dims[[1]]) == 1) stop("1-d vars recognized are not recognized")
  if (length(v$dims[[1]]) >= 2){
    d2 <- v$dims[[1]][2]
    if (d2 == "time"){
      start <- c(start, time[1])
      count <- c(count, 1)
    } else if (d2 =="siglev" || d2 == "siglay"){
      start <- c(start, y[1])
      count <- c(count, 1)
    } else {
      stop("use another means to access var:", var)
    }
  }
  if (length(v$dims[[1]]) == 3){
    d3 <- v$dims[[1]][2]
    if (d3 == "time"){
      start <- c(start, time[1])
      count <- c(count, 1)
    } else if (d3 =="siglev" || d3 == "siglay"){
      start <- c(start, y[1])
      count <- c(count, 1)
    } else {
      stop("use another means to access var:", var)
    }
  }

  dplyr::tibble(node,
                !!var := ncdf4::ncvar_get(x, var, start = start, count = count))
}


#' Get variables for a set of elements.  This is a convenience
#' function that provides a means for getting a vector of values at a given
#' sigma layer (or level) and time.  Note that the dimensions a
#' variable uses varies - see \code{\link{list_vars}}.
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param var charcater the name of the variable.  Not all variables are handled
#'   by this function.
#' @param elem integer one or more 1-based node ID(s).  These must form
#'   a contiguous sequence of elems identifers
#' @param y integer a single 1-based index into siglay, or sigelev
#' @param time either a single 1-based integer of POSIXct
#' @param y_transform logical, if convert y from sigma to integer?
#'    Note that time is auto-converted
#' @return tibble of element id and values - one row for each node
get_elem_var <- function(x, var = c("u", "v"),
                    elem = seq_len(fvcom_count(x, "elems")),
                    y = 1,
                    time = 1,
                    y_transform = FALSE){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  if (length(var) > 1){
    vv <- lapply(var,
      function(v){
        get_elem_var(x, var = v, elem = elem, y = y, time = time, y_transform = y_transform)
      })
    r <- Reduce(function(a,b) dplyr::left_join(a, b, by = "elem"), vv)
    return(r)
  }

  v <- list_vars(x, collapse = FALSE) %>%
      dplyr::filter(.data$name == var[1])
  if (nrow(v) == 0) stop("variable not found:", var[1])
  if (inherits(time, 'POSIXct')) time <- time_index(x, time)
  if (y_transform) y <- sigma_index(x, y, what = v$name[1][2])

  start <- elem[1]
  count <- length(elem)
  if (length(v$dims[[1]]) == 1) stop("1-d vars recognized are not recognized")
  if (length(v$dims[[1]]) >= 2){
    d2 <- v$dims[[1]][2]
    if (d2 == "time"){
      start <- c(start, time[1])
      count <- c(count, 1)
    } else if (d2 =="siglev" || d2 == "siglay"){
      start <- c(start, y[1])
      count <- c(count, 1)
    } else {
      stop("use another means to access var:", var)
    }
  }
  if (length(v$dims[[1]]) == 3){
    d3 <- v$dims[[1]][2]
    if (d3 == "time"){
      start <- c(start, time[1])
      count <- c(count, 1)
    } else if (d3 =="siglev" || d3 == "siglay"){
      start <- c(start, y[1])
      count <- c(count, 1)
    } else {
      stop("use another means to access var:", var)
    }
  }

  dplyr::tibble(elem,
                !!var := ncdf4::ncvar_get(x, var, start = start, count = count))
}


#' Convert a time to a time-index
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param time one or more POSIXct times - UTC
#' @return 1-based index
time_index <- function(x,
                       time = as.POSIXct(
                         paste(Sys.Date(), "12:00:00"),
                         format = '%Y-%m-%d %H:%M:%S',
                         tz = 'UTC')){
  ix <- findInterval(fvcom_time(x), time)
  ix[ix < 1] <- 1
}

#' Convert a time to a sigma level or sigma layer
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param sigma one or more sigma values
#' @param what character either 'siglev' or 'siglay'
#' @return 1-based index
sigma_index <- function(x,
                       sigma = seq(-1,0, by = 0.1),
                       what = c("siglev", "siglay")){
  v <- x$dim[[what]]
  findInterval(sigma, v)
  ix[ix < 1] <- 1
  ix
}

#' Generate a listing of variables and dimensions
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param collapse logical, if TRUE collapse the dim names into a single character
#' @return handy tibble
list_vars <- function(x, collapse = TRUE){
  vars <- names(x$var)
  dims <- lapply(vars,
                 function(v){
                   sapply(x$var[[v]]$dim, '[[', 'name')
                 })
  if (collapse) dims <- unlist(sapply(dims, paste, collapse = ","))
  dplyr::tibble(name = vars, dims)
}
