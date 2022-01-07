



#' Get one or more variables by [element, siglay, time]
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param vars character one or more variable names to retrieve
#' @param indices 3 element indices of the element volume to query or a matrix
#'   of three columns
#' @return matrix
get_element_vars <- function(x, vars = c("u", "v", "ww"),
                             indices = matrix(c(1,100, 200, 300,
                                                1,3, 5, 8,
                                                1,2,3,4),
                                              ncol = 3)){

  v <- list_vars(x) |>
    dplyr::filter(.data$name %in% vars)
  ix <- vars %in% v$name
  if (!all(ix)){
    stop("one or more vars are not known:", paste(vars[!ix], collapse = ", "))
  }
  ix <- grepl("nele", v$dim1, fixed = TRUE)
  if (!all(ix)) stop("vars must be dependent upon elements")

  if (!inherits(indices, "matrix")) indices = matrix(indices, ncol = 3, nrow = 1,  byrow = TRUE)

  counts <- rep(1, ncol(indices))
  sapply(vars,
         function(vname){
           sapply(seq_len(nrow(indices)),
            function(i){
              start <- as.vector(indices[i, , drop = TRUE])
              ncdf4::ncvar_get(x, vname,
                               start = start,
                               count = counts)
            })
         }, simplify = TRUE)
}


#' Retrieve node or element based polygon mesh
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param vars character one or more variable names to retrieve
#' @param lut a table of variable descriptions - see \code{\link{list_vars}}
#' @param mesh the mesh geometry
#' @param ... further arguments for \code{\link{get_node_mesh}} or \code{\link{get_elem_mesh}}
#' @return sf object with mesh geometry and the variables
get_mesh <- function(x,
                     vars = c("zeta", "u", "v"),
                     lut = list_vars(x),
                     mesh = get_elem_mesh_geometry(x, ordered = FALSE),
                     ...){

  for (var in rev(vars)){
    z <- lut |>
      dplyr::filter(.data$name == var)

    mesh <- switch(tolower(z$dim1[1]),
      'nele' = get_elem_mesh(x, vars = var, mesh = mesh, ...),
      'node' = get_node_mesh(x, vars = var, mesh = mesh, ...),
      stop("variable is not defined at nodes or elements: ", var))
  }
  mesh
}

#' Retrieve the node-based mesh with assigned values
#'
#' @param x FVCOM ncdf4 object
#' @param vars character one or more variable names
#' @param mesh the mesh geometry
#' @param fun function to summarize the variable for each polygon
#' @param ... further arguments for selecting \code{get_node_var}
#' @param na.rm logical
#' @return sf object with mesh geometry and the var (computed as fun of nodes)
get_node_mesh <- function(x,
                          vars = 'zeta',
                          mesh = get_node_mesh_geometry(x, ordered = FALSE),
                          fun = mean,
                          na.rm = TRUE,
                          ...){
  v <- get_node_var(x, vars, ...)
  m <- as.matrix(mesh |>
                   sf::st_set_geometry(NULL) |>
                   dplyr::select(.data$p1, .data$p2, .data$p3))
  for (var in vars){
    if (var %in% names(mesh)){
      mesh[[var]] <- sapply(seq_len(nrow(mesh)),
                               function(i){
                                 fun((v |>
                                        dplyr::slice(m[i,]))[[var]], na.rm = na.rm)
                               })
    } else {
      mesh <- mesh |>
        dplyr::mutate(!!var :=
                      sapply(seq_len(nrow(mesh)),
                             function(i){
                               fun((v |>
                                     dplyr::slice(m[i,]))[[var]], na.rm = na.rm)
                             })) |>
        dplyr::relocate(dplyr::all_of(var), .before = "p1")
    }
  }
  mesh
}


#' Retrieve the element-based polygon mesh with assigned values
#'
#' @param x FVCOM ncdf4 object
#' @param vars character one or more variable names
#' @param mesh the mesh geometry
#' @param ... further arguments for \code{\link{get_elem_var}}
#' @return sf object with mesh geometry and the variables
get_elem_mesh <- function(x,
                          vars = c("u", "v"),
                          mesh = get_elem_mesh_geometry(x, ordered = FALSE),
                          ...){

  v <- get_elem_var(x, vars, ...) |>
    dplyr::select(-.data$elem)
  for (var in vars){
    if (var %in% names(mesh)){
      mesh[[var]] <- v[[var]]
    } else {
      mesh <- mesh |>
        dplyr::mutate(!!var := v[[var]]) |>
        dplyr::relocate(dplyr::all_of(var), .before = "p1")
    }
  }
  mesh
}


#' Compute mesh (polygons) for nodes or elements
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param where character, either "nodes" or "elems" (default)
#' @param ... further arguments for \code{\link{get_elem_mesh_geometry}} or \code{\link{get_node_mesh_geometry}}
#' @return sf collection of POLYGON with the following variable
#' \itemize{
#'   \item{p1, p2, p3 the point identifiers for the three points defining the polygon}
#'   \item{geometry 4 point polygon (closed so first and last the same)}
#' }
get_mesh_geometry <- function(x,
                              where = c("nodes", "elems")[2],
                              ...){
  switch(tolower(where[1]),
    "elems" = get_elem_mesh_geometry(x, ...),
    "elem"  = get_elem_mesh_geometry(x, ...),
              get_node_mesh_geometry(x, ...))

}

#' Compute the mesh (polygons) for elements
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param what character either 'lonlat' or 'xy'
#' @param crs character (as "auto") or something suitable for \code{\link[sf]{st_crs}}
#' @param ... further arguments for \code{\link{distinct_polygons}}
#' @return sf collection of POLYGON with the following variables
#' \itemize{
#' \item{elem the element identifier}
#' \item{p1, p2, p3 the node identifiers for the three points define the polygon}
#' \item{geometry 4 point polygon (closed so first and last the same)}
#' }
get_elem_mesh_geometry <- function(x, what = 'lonlat', crs = "auto", ...){

  if (tolower(crs[1]) == "auto"){
    crs <- switch(tolower(what[1]),
                  "lonlat" = fvcom_crs(what = "lonlat", form = "epsg"),
                  fvcom_crs(what = "xy", form = "wkt"))
  }

  # nodes around each element
  # make a matrix, and then wrap
  p <- distinct_polygons(x, where = 'elems', ...)
  m <- as.matrix(p)
  m <- cbind(m, m[,1])
  N <- nrow(m)
  nodes <- fvcom_nodes(x, form = 'table', what = what)
  xy <- nodes |>
    dplyr::select(-.data$node) |>
    as.matrix()
  nm <- colnames(xy)
  g <- lapply(seq_len(nrow(m)),
    function(i){
      sf::st_polygon(list(xy[m[i,],]))
    })


  p |>
    dplyr::mutate(geometry = g) |>
    sf::st_sf(sf_column_name = "geometry", crs = crs) |>
    dplyr::mutate(elem = seq_len(N)) |>
    dplyr::relocate(.data$elem, .before = 1)

}

#' Compute the mesh (polygons) for nodes
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param what character either 'lonlat' or 'xy'
#' @param crs character (as "auto") or something suitable for \code{\link[sf]{st_crs}}
#' @param ... further arguments for \code{\link{distinct_polygons}}
#' @return sf collection of POLYGON with the following variable
#' \itemize{
#' \item{node node identifier}
#' \item{p1, p2, p3 the node identifiers for the three points define the polygon}
#' \item{geometry 4 point polygon (closed so first and last the same)}
#' }
get_node_mesh_geometry <- function(x, what = 'lonlat', crs = "auto", ...){


  if (tolower(crs[1]) == "auto") crs <- fvcom_crs(what[1])

  p <- distinct_polygons(x, where = 'nodes', ...)
  m <- as.matrix(p)
  N <- nrow(m)
  nodes <- fvcom_nodes(x, form = 'table', what = what)
  xy <- nodes |>
    dplyr::select(-.data$node) |>
    as.matrix()
  nm <- colnames(xy)
  g <- lapply(seq_len(nrow(p)),
    function(i){
      ix <- c(m[i,], m[i,1])
      sf::st_polygon(list(xy[ix,]))
    })

  p |>
    dplyr::mutate( geometry = g) |>
    sf::st_sf(sf_column_name = "geometry", crs = crs) |>
    dplyr::mutate(node = seq_len(N)) |>
    dplyr::relocate(.data$node, .before = 1)

}


#' Compute a table of distinct polygons (triangles) by listing the
#' nodes (or elements).
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param where character, either 'nodes' or "elems"
#' @param ordered logical if TRUE then order the polygons by node id (low to high)
#' @return ordered tibble of p1, p2, and p3
distinct_polygons <- function(x,
    where = c("nodes", "elems")[1],
    ordered = FALSE){

  if (tolower(where[1]) == "nodes"){
    n   <- ncdf4::ncvar_get(x, "ntsn")
    nbs <- ncdf4::ncvar_get(x, "nbsn")
    nav <- cbind(nbs, n)
    nc <- ncol(nav)
    # generate a lost of matrices for each node - there will be replicates
    ns <- lapply(seq_len(nrow(nav)),
              function(i){
                t(sapply(seq(from = 2, to = nav[i,nc]-2) ,
                             function(j){
                               c(i, nav[i,j:(j+1)])
                             }))
              })
    # join them, sort by node number (across rows)
    ns <- do.call(rbind, ns)
    ns <- t(apply(ns, 1, sort))
    # convert to tibble
    # retain only the unique ones
    colnames(ns) <- paste0("p", 1:3)
    r <- dplyr::as_tibble(ns) |> dplyr::distinct_all()
  } else {
    nv <- ncdf4::ncvar_get(x, "nv")
    colnames(nv) <- paste0("p", 1:3)
    r <- dplyr::as_tibble(nv)
  }

  if (ordered) r <- r |> dplyr::arrange(.data$p1, .data$p2, .data$p3)

  r
}

#' Retrieve bathymetry "sea_floor_depth_below_geoid" in meters
#' with positive downward.
#'
#' @export
#' @param x ncdf4 object
#' @param what character either 'lonlat' or 'xy'
#' @param form character specifies output form "table", "raster" or "mesh"
#' @return something
bathymetry <- function(x,
                       what = c("lonlat", "xy")[1],
                       form = c("table", "raster", "mesh")[1]){

  xyz <- dplyr::bind_cols(
    fvcom_nodes(x, what = what),
    dplyr::tibble(bathymetry = ncdf4::ncvar_get(x, "h")))
  if (tolower(form[1]) == "raster"){
    xr <- range(xyz$lon)
    yr <- range(xyz$lat)
    r <- 0.01
    template <- raster::raster(res = r,
                       xmn = xr[1] - r/2,
                       xmx = xr[2] + r/2,
                       ymn = yr[1] - r/2,
                       ymx = yr[2] + r/2,
                       crs = fvcom_crs(what[1]))
    xyz <- sf::st_as_sf(xyz, coords = c("lon", "lat"),
                        crs = fvcom_crs(what[1]))
    xyz <- raster::rasterize(xyz, template,
                             field = "bathymetry",
                             fun = mean)

  } else if (tolower(form[1]) == "mesh"){
    # uhh?
  }
  xyz
}


#' Determine if elements on the boundary, and if so if open or closed
#'
#' @export
#' @param x ncdf4 object
#' @param threshold numeric, the depth at which to consider an edge open (deep) or closed (shallow)
#' @return tibble with four variables \code{elem}, \code{h_center}, \code{edge}, \code{bound}
fvcom_bounds <- function(x, threshold = 1){
  
  edge <- ncdf4::ncvar_get(x, "nbe") |>
    apply(1, function(x) any(x < 1))
  h <- fvcom::get_elem_var(x, "h_center")
  shallow <- h$h_center < threshold
  closed <- shallow & edge
  open <- !shallow & edge
  bound <- rep(NA_character_, nrow(h))
  bound[closed] <- "closed"
  bound[open] <- "open"
  
  h |>
    dplyr::mutate(edge = edge,
                  bound = bound)
}