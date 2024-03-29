#' Test is an object is of ncdf4 class
#'
#'
#' @export
#' @param x object to be tested
#' @return logical TRUE if the object is a valid ncdf4 class
is_ncdf4 <- function(x){
  inherits(x, "ncdf4")
}


#' Generate random points within the element or node space.
#'
#' A random point may fall outside of the envelope of elements/nodes since
#' we use a simple bounding box for each dimension.  Be sure to check your
#' points, if that is important, with \code{\link[sf]{st_contains}}.
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param n the number of points to generate
#' @param what either 'lonlat' or 'xy'
#' @param ... arguments for \code{\link{fvcom_time}}
#' @param form character either 'tibble' or 'sf'
#' @return tibble of
#' \itemize{
#' \item{x x coordinate, possibly lon}
#' \item{y y coordinate, possibly lat}
#' \item{z z coordinate as sigma value} 
#' \item{time time ast POSIXct}
#' }
#' or an sf POINT object where xy are the geometry
fvcom_random <- function(x, n = 1,
                         what = c("longlat", "xy")[1],
                         form = c("tibble", "sf")[1], 
                         ...){



  rand <- function(v, n = 1) {
    if (inherits(v, "POSIXt")){
      t0 <- v[1]
      v <- as.numeric(v)
      vd <- v - v[1]
      r <- runif(n, min = vd[1], max = vd[2]) + t0
    } else {
      r <- runif(n, min = v[1], max = v[2])
    }
    return(r)
  }
  

  if (tolower(what[1]) =='xy'){
    xr <- range(ncdf4::ncvar_get(x, "x"))
    yr <- range(ncdf4::ncvar_get(x, "y"))
  } else {
    xr <- range(ncdf4::ncvar_get(x, "lon"))
    yr <- range(ncdf4::ncvar_get(x, "lat"))
  }
  
  
  zr <- range(x$dim$siglev$vals[1,])

  timer <- range(fvcom::fvcom_time(x, ...))

  r <- dplyr::tibble(
    x = rand(xr, n = n),
    y = rand(yr, n = n),
    z = rand(zr, n = n),
    time = rand(timer, n = n))

  if (tolower(form[1]) == 'sf'){
    r <- sf::st_as_sf(r,
                      coords = c("x", "y", "z")) |>
      sf::st_set_crs(fvcom_crs(what = what[1]))
  }

  r
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
#' @param where character, either 'nodes'( or 'node'), 'elems' (or 'elem'),
#'   'siglay', 'siglev', or 'time'
#' @return  number of nodes or elements
fvcom_count <- function(x, where = c("nodes", "elems", "siglay", "siglev", "time")[1]){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  switch(tolower(where[1]),
         'node' = x$dim$node$len,
         'nodes' = x$dim$node$len,
         'siglay' = x$dim$siglay$len,
         'siglev' = x$dim$siglev$len,
         'time' = x$dim$time$len,
         x$dim$nele$len)
}

#' Generate a random sample of element [elem, siglay,time] or node [node, siglev, time] indices
#'
#' @export
#' @param x ncdf4 FVCOM object
#' @param n numeric, the number of IDs to retrieve
#' @param where character, either 'nodes' or 'elems' (default)
#' @return named vector of random indices
fvcom_sample <- function(x, n = 1, where = c("elems", "nodes")[1]){
  dnames <- switch(tolower(where[1]),
   #[elem, siglay, time]
   'elems' = c("elems", "siglay", "time"),
   'nodes' = c("nodes", "siglev", "time"),
    stop("where must be 'elems' or 'nodes'"))


   sapply(dnames,
          function(dname){
            sample(fvcom::fvcom_count(x, where = dname), size = n, replace = FALSE)
          })

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
                                       y = ncdf4::ncvar_get(x, varid = "y")) |>
                         dplyr::rename(x = .data$x1) )

  if (inherits(include, "character")){
    include <- tolower(include)
    if ("art2" %in% include){
      r <- r |>
        dplyr::mutate(art2 = ncdf4::ncvar_get(x, varid = "art2")) |>
        dplyr::relocate(dplyr::all_of("art2"), .after = "node")
    }
    if ("art1" %in% include){
      r <- r |>
        dplyr::mutate(art1 = ncdf4::ncvar_get(x, varid = "art1"))  |>
        dplyr::relocate(dplyr::all_of("art1"), .after = "node")
    }
    if ("nbs" %in% include){
      ntsn   <- ncdf4::ncvar_get(x, "ntsn")
      nbsn <- ncdf4::ncvar_get(x, "nbsn")
      nav <- cbind(nbsn, ntsn)
      n <- ncol(nav)
      r <- r |>
        dplyr::mutate(nbs = apply(nav, 1, function(x) x[1:(x[n])] )) |>
        dplyr::relocate(dplyr::all_of("nbs"), .after = "node")
    }
  }

   if (!is.null(index)){
     r <- r |>
       dplyr::slice(index)
   }

   if (tolower(form[1]) == 'sf'){
     coords <- switch(tolower(what),
                      "xy" = c("x", "y"),
                      c("lon", "lat"))
     r <- r |>
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
#'   \item{nbs include a list other element-wise information.  If not  `none`` then consider
#'         `nv` for indices of neighbor nodes and/or `nbe` for indices of neighbor elements}
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
                        include = "none"){

  if (!is_ncdf4(x)) stop("input must be ncdf4 class object")

  idx <- seq_len(fvcom_count(x, where = 'elems'))
  r <- switch(tolower(what[1]),
         'lonlat'= dplyr::tibble(elem = idx,
                                 lon = ncdf4::ncvar_get(x, varid = "lonc"),
                                 lat = ncdf4::ncvar_get(x, varid = "latc")),
         'xy'    = dplyr::tibble(elem = idx,
                                 x1 = ncdf4::ncvar_get(x, varid = "xc"),
                                 y = ncdf4::ncvar_get(x, varid = "yc")) |>
                   dplyr::rename(x = .data$x1) )

  if (inherits(include, "character") && include[1] != "none"){

    include <- tolower(include)

    do_mutate <- function(r, inc = "foo", x = NULL){
      r <- try(
          r |>
            dplyr::mutate(!!inc := ncdf4::ncvar_get(x, inc))
      )
      if (inherits(r, "try-catch")){
        print(r)
        stop("error retrieving:", inc)
      }
      r
    }

    for (inc in include) r <- do_mutate(r, inc, x)

  }

   if (!is.null(index)){
     r <- r |>
       dplyr::slice(index)
   }

   if (tolower(form[1]) == 'sf'){
     coords <- switch(tolower(what),
                      "xy" = c("x", "y"),
                      c("lon", "lat"))
     r <- r |>
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
#' @param x FVCOM ncdf4 object
#' @param what character either 'xy', 'lonlat' or 'longlat'.  Actually, if not 'xy'
#'   then 'lonlat' is returned.
#' @param form character, one of 'wkt', 'epsg' or 'proj' By default 'wkt' if \code{what} is
#'   'xy' and 'epsg' otherwise.
#' @return character CRS
fvcom_crs <- function(x,
                      what = c("lonlat", "xy")[1],
                      form = c("wkt", "epsg", "proj")[ifelse(tolower(what[1]) == "xy", 1, 2)]){

  # https://spatialreference.org/ref/esri/102284/
  # nad83:1802 -> epsg::102684 (feet)
  # nad83:1802 -> epsg::102284 (meters)
  #default values
  epsg <- c(xy = 'EPSG:102284',
            lonlat = "EPSG:6318" ) #'EPSG:4326')  #https://epsg.io/6318
  wkt <- c(xy = "PROJCS[\"unknown\",GEOGCS[\"unknown\",DATUM[\"North_American_Datum_1983\",SPHEROID[\"GRS 1980\",6378137,298.257222101,AUTHORITY[\"EPSG\",\"7019\"]],AUTHORITY[\"EPSG\",\"6269\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"latitude_of_origin\",42.8333333333333],PARAMETER[\"central_meridian\",-70.1666666666667],PARAMETER[\"scale_factor\",0.999966666666667],PARAMETER[\"false_easting\",900000],PARAMETER[\"false_northing\",0],UNIT[\"metre\",1,AUTHORITY[\"EPSG\",\"9001\"]],AXIS[\"Easting\",EAST],AXIS[\"Northing\",NORTH]]",
           lonlat = "GEOGCRS[\"WGS 84\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"unknown\"],\n        AREA[\"World\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"EPSG\",4326]]")
  proj <- c(xy = "proj=tmerc +datum=NAD83 +lon_0=-70d10 lat_0=42d50 k=.9999666666666667 x_0=900000 y_0=0",
            lonlat = "+proj=longlat +datum=NAD83")

  if (!missing(x)){
    att <- ncdf4::ncatt_get(x, varid = 0)
    if ("CoordinateProjection" %in% names(att)){
      proj[["xy"]] <- att$CoordinateProjection
      wkt[["xy"]] <- sf::st_crs(proj[['xy']])
    }
  }


  wh <- tolower(what[1])
  switch(tolower(form[1]),
    "epsg" = epsg[[wh]],
    "wkt" = wkt[[wh]],
    "proj" = proj[[wh]]
  ) # switch on form
}

#' Get variables for nodes or elements.  It is not possible to get a mix of
#' variables - that is some from nodes and some from elements - in one call.
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param vars character - one or more variables
#' @param lut a table of variable descriptions - see \code{\link{list_vars}}
#' @param ... further arguments for \code{\link{get_node_var}} or \code{\link{get_elem_var}}
#' @return list of variables - see \code{\link{get_node_var}} or \code{\link{get_elem_var}}
get_vars <- function(x,
                    vars = c("u", "v"),
                    lut = list_vars(x),
                    ...){
  lapply(vars,
    function(var){
      z <- lut |>
        dplyr::filter(.data$name == var)
      switch(tolower(z$dim1[1]),
        'nele' = get_elem_var(x, ...),
        'node' = get_node_var(x, ...),
        stop("variable is not defined at nodes or elements:", var))
    })
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

  v <- list_vars(x) |>
      dplyr::filter(.data$name == var[1])
  if (nrow(v) == 0) stop("variable not found:", var[1])
  if (v$dim1 != "node") stop("variable must have node as first dimension")
  if (inherits(time, 'POSIXct')) time <- time_index(x, time)
  if (y_transform) y <- sigma_index(x, y, what = v$name[1][2])

  start <- node[1]
  count <- length(node)
  dims <- strsplit(v$dims[[1]], ",", fixed = TRUE)[[1]]
  if (length(dims) >= 2){
    d2 <- dims[2]
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
  if (length(dims) >= 3){
    d3 <- dims[3]
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
#' @param time either a single 1-based integer or POSIXct
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

  v <- list_vars(x) |>
      dplyr::filter(.data$name == var[1])
  if (nrow(v) == 0) stop("variable not found:", var[1])
  if (v$dim1[1] != "nele") stop("variable must have nele as first dimension")
  if (inherits(time, 'POSIXct')) time <- time_index(x, time)
  if (y_transform) y <- sigma_index(x, y, what = v$name[1][2])

  start <- elem[1]
  count <- length(elem)
  dims <- strsplit(v$dims[[1]], ",", fixed = TRUE)[[1]]
  if (length(dims) >= 2){
    d2 <- dims[2]
    if (d2 == "time"){
      start <- c(start, time[1])
      count <- c(count, 1)
    } else if (d2 =="siglev" || d2 == "siglay"){
      start <- c(start, y[1])
      count <- c(count, 1)
    } else {
      #stop("use another means to access var:", var)
    }
  }
  if (length(dims) >= 3){
    d3 <- dims[3]
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
#' @return handy tibble
list_vars <- function(x){
  vars <- names(x$var)
  Dims <- lapply(vars,
                 function(v){
                   sapply(x$var[[v]]$dim, '[[', 'name')
                 })
  getdim <- function(dims, n = 1){
        sapply(dims,
            function(d){
              if (inherits(d, "character")) {
                r <- ifelse(length(d) >= n, d[[n]], "")
              } else {
                r <- ""
              }
              r
              })
  }
  dplyr::tibble(name = vars,
                dims = unlist(sapply(Dims, paste, collapse = ",")),
                dim1 = getdim(Dims, 1),
                dim2 = getdim(Dims, 2),
                dim3 = getdim(Dims, 3),
                units = sapply(vars, function(v) x$var[[v]]$units ),
                longname = sapply(vars, function(v) x$var[[v]]$longname) )
}

#' Given an element, get its neighboring nodes
#' 
#' @export
#' @param x FVCOM ncdf4 object
#' @param elem numeric, element ID
#' @return 4 column matrix where columns are [elemID, pID1, pID2, nodeID3]
get_element_nodes <- function(x, elem = 1:3){
  r <- cbind(elem, 
       sapply(elem,
         function(elemID){
           n <- ncdf4::ncvar_get(x, "nv", start = c(elemID, 1), count = c(1, 3))
         }))
  colnames(r) <- c("elem", "p1", "p2", "p3")
  r
}
