#' A class for navigating  FVCOM datasets
#' 
#' @description R6 base class suitable for inheritance by other classes
#' @export
FVCOM_Physics <- R6::R6Class("FVCOM_Physics",
  
  portable = TRUE, 
  
  public = list(
    #' @field filename character, path or URL
    filename = NULL,
    #' @field NC ncdf4 object
    NC = NULL,
    #' @field M sf table of mesh
    M = NULL,
    #' @field t0 POSIXct timestamp identifying the start time
    t0 = NULL,
    #' @field verbose logical for helpful messaging
    verbose = NULL,
    #' @field zvar character, the variable to use as 'z' such as siglev or siglay
    zvar = NULL,
    #' @param filename character either a filename or OpenDAP URL
    #' @param origin POSIXct timestamp indicating the start of the experiment
    #' @param crs 
    #' @param verbose logical for helpful messaging
    #' @param zvar character, the variable to use as 'z' such as siglev or siglay
    initialize = function(filename,
                          origin = as.POSIXct("1858-11-17 00:00:00", tz = 'UTC'),
                          verbose = FALSE,
                          zvar = c('siglev', 'siglay')[1]){
      self$filename <- filename[1]
      self$verbose <- verbose[1]
      self$zvar <- zvar[1]
      self$t0 <- origin[1]
      private$message("opening NCDF4")
      self$NC <- try(ncdf4::nc_open(self$filename))
      if (inherits(self$NC, "try-error")) stop("unable to open NCDF4 resource:", filename[1])
      private$message("retrieving mesh")
      self$M <- fvcom::get_mesh_geometry(self$NC, where = 'elems', what = 'xy',
                                         crs = self$get_crs(form = 'wkt'))
      ok <- self$append_bounds()
    },
    
    #' @description Pretty print
    print = function(){
      cat(class(self)[1], "class:", self$filename, "\n")
      cat("  CRS        :", self$get_crs(), "\n")
      cat("  zvar       :", self$zvar, "\n")
      cat("  t0         :", format(self$t0, "%Y-%m-%dT%H:%M:%S %Z"), "\n")
      cat("  n timesteps:", self$NC$dim$time$len, "\n")
      cat("  n elements :", self$NC$dim$nele$len, "\n")
      cat("  n nodes    :", self$NC$dim$node$len, "\n")
      cat("  verbose    :", self$verbose, "\n")
      
    }, 
    
    
    #' @description Retrieve the CRS
    #' @param form character, one of 'proj', 'wkt'
    #' @return CRS in the specified form
    get_crs = function(form = c("proj", "wkt")[1]){
      crs <- ncdf4::ncatt_get(self$NC, 0)[["CoordinateProjection"]]
      if (length(crs) > 0 && crs[1] != "none"){
        if (substring(crs, 1, 1) != "+") crs <- paste0("+", crs)
        if (tolower(form[1]) == 'wkt') crs <- sf::st_crs(crs)
      } else {
        crs <- NA
        if (tolower(form[1]) == 'wkt') crs <- sf::st_crs()
      }
      crs
    },
    
    #' @description retrieve the time relative to some epoch/origin
    #' @param origin POSIXct the orogin of relative timestamps
    #' @param ... other arguments for \code{\link[fvcom]{fvcom_time}}
    get_time = function(origin = self$t0, ...){
      fvcom::fvcom_time(self$NC, origin = origin[1], ...)
    },
    
    #' @description Retriev one or more random points
    #' @param n numeric, the number of points to generate
    #' @param select_time character, one of 'first' (default), 'last' or 'random'
    #' @param select_z numeric or character.  If character then 'random' (default), 'surface', 'bottom'.
    #'   If numeric then you specify your own in \code{zvar} units
    #' @return sf object of class POINT
    random_points = function(n = 1, 
                             select_time = c("first", "last", "random")[1],
                             select_z = c("random", "surface", "bottom")[1]){
      random_point(self, 
                   n = n, 
                   select_time = select_time, 
                   zvar = self$zvar, 
                   select_z = select_z)
    },
    
    #' @description find proxy for characteristic size based upon element size
    #'
    #' NOTE if we update GEOS we could get size of inscribed circle
    #'
    #' @param x element mesh as sf
    #' @param what character
    #' \itemize{
    #' \item min or square - side length of sqaure with equivalent area
    #' \item max - side of square with double the area (or 1.4*sqrt(area))
    #' \item circle - radius of circle with equivalent area
    #'  }
    #' @return numeric characteristic size
    char_size = function(what = c("min", "max", "square", "circle")[4]){
      area <- sf::st_area(self$M)
      # area <- sf::st_inscribed_circle(x) |> sf::str_area()
      switch(tolower(what[1]),
             "min" = sqrt(area),
             "max" = sqrt(2*area),
             "circle" = sqrt(area/pi),
             "square" = sqrt(area))
    },
    
    #' @description generate kinematics metrics for a given mesh
    #'
    #' @param ofile optional file to save to (as non-spatial CSV) or NA/NULL to skip
    #' @return mesh (invisibly) with variables added including
    #' \itemize{
    #' \item max_u, max_v, max_w maximum velocity by element over all sigma in m/s
    #' \item area element area in m^2
    #' \item char_dim characteristic dimension, for now the radius of the circle with equivalent area in m
    #' }
    mesh_metrics = function(ofile = c(NA, file.path(dirname(self$filename), "mesh-elem-metrics.csv.gz"))[1]){
      self$M$area <- sf::st_area(self$M)
      self$M$char_dim <- self$char_size()
      get_abs_max <- function(x){max(abs(x), na.rm = T)}
      self$M$max_u <- apply(ncdf4::ncvar_get(self$NC, "u"), 1, get_abs_max)
      self$M$max_v <- apply(ncdf4::ncvar_get(self$NC, "v"), 1, get_abs_max)
      self$M$max_w <- apply(ncdf4::ncvar_get(self$NC, "ww"), 1, get_abs_max)
      if (length(ofile[1]) > 0 && !is.na(ofile[1])){
        sf::st_drop_geometry(self$M) |>
          readr::write_csv(ofile[1])
      }
      invisible(self$M)
    },
    
    #' @description Compute the mean depth of each mesh element
    #'
    #' @return mesh (invisibly) with depth added
    mesh_depth = function(){
      h <- ncdf4::ncvar_get(self$NC, "h")
      nodes <- as.matrix(self$M |> 
                           sf::st_drop_geometry() |> 
                           dplyr::select(dplyr::all_of(c("p1", "p2", "p3"))))
      self$M <- dplyr::mutate(self$M, 
                              depth = apply(nodes, 1, function(nn){ mean(h[nn]) }),
                              .before = dplyr::all_of("geometry"))  
      self$M
    },
    
    
    #' @description append the boundary info to the mesh table
    #' 
    #' @return mesh invisibly with appended boundary variable where
    #' \itemize{
    #'  \item{open indicates the element is boundaed on at least one side by open water}
    #'  \item{closed indicates the element is bounded on at least one side by shoreline}
    #'  \item{internal indicates the element is not on the boundary}
    #'  \item{unknown self-explanatory}
    #' }
    append_bounds = function(){
      self$M <- dplyr::mutate(self$M, bounds = "unknown") |>
        dplyr::relocate(dplyr::starts_with("geometry"), .after = dplyr::last_col())
      invisible(self$M)
    },
    
    #' @description Plot element mesh
    #' 
    #' @seealso \code{\link{plot_mesh}} and \code{\link{plot_mesh_geometry}}
    #' 
    #' @param ... other arguments for \code{\link[sf]{plot}}
    plot = function( ...){
      plot_mesh(self, ...)
    }
    
    
  ), # public
                          
  active = list(
    #' @field open_bounds provides the indices for open boundary elements (open water)
    open_bounds = function(){
      # typically one might read from a file here, for example...
      # readRDS(system.file("extdata/elem_open.Rds", package = "necofs"))
      # where the return value provides the indices for open bounded elements
      # but instead, we return NULL in the generic case
      NULL
    },
    #' @field closed_bounds provides the indices for closed boundary elements (shore)
    closed_bounds = function(){
      # typically one might read from a file here, for example...
      # readRDS(system.file("extdata/elem_closed.Rds", package = "necofs"))
      NULL
    }
  ), # active
                          
  private = list(
    finalize = function(){
      ncdf4::nc_close(self$NC)
    },
    message = function(fmt, ...){
      if (self$verbose){
        if (missing(...)){
          cat(fmt, sep = "\n")
        } else {
          cat( sprintf(fmt, ...), sep = "\n")
        }
      } 
      invisible(NULL)
    }
  ) # private
                          
) #FVCOMPhysics


#' Given a set of points within the mesh, interpolate a variable such as bathymetry.
#' 
#' Note that some variables are colocated with nodes, while others are associated
#' with elements (triangular faces).  The former require interpolation, the later requires
#' more prep but no interpolation. The first dimension of any requested variable must
#' be 'node' (implemented) or "nele" (not implemented).  Any other is an error.
#'
#' Nodes: We could just find the mean var of the three nodes surrounding each element,
#' but that assumes the point in in the centroid of the element which is likely to 
#' not be the case.  So, this uses simple plane geometry to interpolate the depth
#' at an arbitrary location within the mesh.
#'
#' @export
#' @param p sf POINT object
#' @param X FVCOMPhysics object with an element mesh
#' @param var character the variable to interpolate (bathymetry is h)
#' @return numeric vector of interpolates, NA where points fall outside of the mesh
interpolate_var <- function(p, X, var = 'h'){
  
  vars <- list_vars(X$NC)
  dim1 <- vars |>
    dplyr::filter(.data$name == var) |>
    dplyr::pull(dplyr::all_of("dim1"))
  
  # boolean vector of which elements contain each point 
  # which we then convert to indices
  ix <- sf::st_intersects(p,X$M)
  ix_elems <- sapply(seq_len(length(ix)),
                       function(i) {
                         r <- unlist(ix[[i]])
                         if (length(r) == 0) r <- NA_integer_
                         r}
                     )
  
  outside <- is.na(ix_elems)
  if (any(outside)){
    warning("one or more points outside of mesh")
  }
  
  if (dim1 == "node"){
    # interpolate by getting the three surrounding nodes
    # for each element
    #  get the nodes
    #   for each node get the depth
    #   interpolate the depth at point p
    #  http://paulbourke.net/geometry/pointlineplane/
    # Ax + By + Cz + D = 0
    # A = y1 (z2 - z3) + y2 (z3 - z1) + y3 (z1 - z2)
    # B = z1 (x2 - x3) + z2 (x3 - x1) + z3 (x1 - x2)
    # C = x1 (y2 - y3) + x2 (y3 - y1) + x3 (y1 - y2)
    # - D = x1 (y2 z3 - y3 z2) + x2 (y3 z1 - y1 z3) + x3 (y1 z2 - y2 z1)
    # 
    # z <- (Ax + By + D)/(-C)
    
    get_interp_var <- function(i, p, X, ix_elems){
      ix_elem <- ix_elems[i]
      m <- X$M |> dplyr::slice(ix_elem) 
      ix_nodes <- c(m$p1, m$p2, m$p3)
      nodes <- fvcom_nodes(X$NC, index = ix_nodes, what = "xy") |>
        dplyr::left_join(get_node_var(X$NC, var = var, node = ix_nodes), by = "node")
      x <- nodes$x
      y <- nodes$y
      z <- nodes[[var]]
      A <- y[1]*(z[2] - z[3]) + y[2]*(z[3] - z[1]) + y[3]*(z[1] - z[2])
      B <- z[1]*(x[2] - x[3]) + z[2]*(x[3] - x[1]) + z[3]*(x[1] - x[2])
      C <- x[1]*(y[2] - y[3]) + x[2]*(y[3] - y[1]) + x[3]*(y[1] - y[2])
      D <- -(x[1]*(y[2]*z[3] - y[3]*z[2]) + x[2]*(y[3]*z[1] - y[1]*z[3]) + x[3]*(y[1]*z[2] - y[2]*z[1]))
      xy <- sf::st_coordinates(p |> dplyr::slice(i))
      (A*xy[1,'X'] + B*xy[1,"Y"] + D)/(-C)
    }
    
    r <- rep(NA_real_, nrow(p))
    for (i in seq_len(nrow(p))){
      if (!outside[i]) r[i] <- get_interp_var(i, p, X, ix_elems)
    }
  } else if (dim1 == "nele") {
    # here we need to slice [time, layer, nelem]
    # we have the info, but we need to take the point time and Z info
    # to interpolate - boo
    stop("not implemented:", var)
  } else {
    stop("var must be node or element based:", var)
  }
  r
}

#' Generate  listing of one or more random points
#' 
#' @export
#' @param X FVCOM_Physics object
#' @param n numeric the number of random points to generate
#' @param select_time character, one of 'first' (default), 'last' or 'random'
#' @param select_z numeric or character.  If character then 'random' (default), 'surface', 'bottom'.
#'   If numeric then you specify your own in \code{zvar} units. If numeric then the
#'   user must provide \code{1} (recycled across all points) or \code{n} values.
#' @param zvar character, the name of the varoiable to use for z locations 
#' @return sf POINT XYZ object with \code{n} features
random_point <- function(X, 
                         n = 1,
                         select_time = c("first", "last", "random")[1],
                         select_z = c("random", "surface", "bottom")[1],
                         zvar = c('siglev', 'siglay')[1]) {
  
  stopifnot(zvar[1] %in% names(X$NC$dim))
  zvar <- zvar[1]
  
  # given a 2 element vector [min,max] compute n random numbers within
  rand <- function(v, n = 1) {
    if (inherits(v, "POSIXt")){
      t0 <- v[1]
      v <- as.numeric(v)
      vd <- v - v[1]
      r <- stats::runif(n, min = vd[1], max = vd[2]) + t0
    } else {
      r <- stats::runif(n, min = v[1], max = v[2])
    }
    return(r)
  }
  
  # the z-range
  zr <- range(X$NC$dim[[zvar]]$vals[1,])
  
  # get the seed points
  p <- sf::st_sample(X$M, n)
  elem <- sf::st_contains(X$M, p, sparse = FALSE) |>
    apply(2, which)
  
  times <- X$get_time()
  thistime <- switch(tolower(select_time[1]),
                     "first" = times[1],
                     "last" = times[length(times)],
                     rand(range(times),n))
  
  # z values (siglay or siglev) are positive upward,
  # so surface/bottom look backwardsy to my eye,
  # if user provides numerics then use those
  if (inherits(select_z, 'character')){
    z <- switch(select_z[1],
                'surface' = zr[2],
                'bottom' = zr[1],
                rand(zr, n))
  } else {
    # the user must provide either 1 or n values
    # we'll let dplyr::mutate catch that later
    z <- select_z
  }
  
  p <- sf::st_coordinates(p) |>
    dplyr::as_tibble() |>
    dplyr::mutate(Z = z,
                  time = thistime, 
                  elem = elem) |>
    dplyr::relocate(dplyr::contains("elem"), .before = 1) |>
    sf::st_as_sf(coords = c("X", "Y", "Z"), 
                 dim = "XYZ",
                 crs = sf::st_crs(p))
  
  # if any are NA then try again
  while(any(is.na(p$elem))){
    ix <- is.na(p$elem)
    nbad <- sum(ix)
    nkeep <- n - nbad
    p <- p |>
      dplyr::filter(!ix) |>
      dplyr::bind_rows(random_point(X, n = nbad))
  }
  p
}

#' Given one or more spatial locations, generate POINT objects to seed for tracking
#'
#' @export
#' @param X FVCOM_Physics object
#' @param x sf POINT or numeric one of more x coordinates such as longitude
#'   if sf POINT class then y, z, time crs are ignored.
#' @param y numeric one of more y coordinates such as latitude
#' @param z numeric one of more z coordinates such as depth
#' @param time POSIXct one of more time stamps (UTC)
#' @param crs the CRS for the input [x,y] coordinates 
#' @return sf POINT object suitable for tracking
as_seed <- function(X, x, y, z = 0, time = X$get_time()[1], crs = 4326){
  
  if (inherits(x, 'sf')){
    p <- sf::st_transform(x, crs = sf::st_crs(X$M)) |>
      dplyr::mutate(elem = point_element(X, rlang::.data), .before = 1)
  } else {
    p <- dplyr::tibble(x = x, y = y, z = z, time = time ) |>
      sf::st_as_sf(coords = c("x", "y", "z"), crs = crs) |>
      sf::st_transform(crs = sf::st_crs(X$M))
    elem <- point_element(X, p)
    p <- dplyr::mutate(p, elem = elem, .before = 1)
  }
  return(p)
}

#' Given a POINT object, determine the elements each point belongs to
#' 
#' @export
#' @param X FVCOM_Physics object
#' @param p sf POINT object
#' @return integer index matching the mesh element that contains each point.  For points
#'   outside of the mesh -1 is returned.
point_element <- function(X, p){
  sf::st_contains(X$M, p, sparse = FALSE) |>
    apply(2, function(x){
      ix <- which(x)
      if (length(ix) <= 0) ix <- -1
      ix
    })
}

#' Instantiate a FVCOM_Physics R6 object
#' 
#' @export
#' @param filename character either a filename or OpenDAP URL
#' @param ... other arguments passed to \code{FVCOM_Physics$new(filename, ...)}
#' @return FVCOM_Physics R6 Reference Object
FVCOMPhysics <- function(filename = file.path("http://www.smast.umassd.edu:8080",
                                               "thredds/dodsC/models/fvcom/NECOFS/Archive/NECOFS_GOM/2019",
                                               "gom4_201901.nc"),
                          ...){
  FVCOM_Physics$new(filename, ...)
}
