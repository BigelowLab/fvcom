#' Retrieve the mesh with assigned values
#'
#' @param x FVCOM ncdf4 object
#' @param vars character name of the variable
#' @param mesh the mesh geometry
#' @param fun function to summarize the variable for each polygon
#' @param ... further arguments for selecting \code{get_node_var}
#' @return sf object with mesh geometry and the var (computed as mean of nodes)
get_node_mesh <- function(x, vars = 'zeta',
                          mesh = get_node_mesh_geometry(x),
                          fun = mean,
                          ...){
  vv <- sapply(vars, function(var) get_node_var(x, var, ...), simplify = FALSE)
  m <- as.matrix(mesh %>%
                   sf::st_set_geometry(NULL) %>%
                   dplyr::select(p1,p2,p3))
  for (var in vars){
    mesh <- mesh %>%
      tibble::add_column(!!var :=
                    sapply(seq_len(nrow(mesh)),
                           function(i){
                             fun((vv[[var]] %>%
                                   dplyr::slice(m[i,]))[[var]])
                           }), .before = "p1")
  }
  mesh
}


#' Compute the mesh (polygons) for nodes
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param what character either 'lonlat' or 'xy'
#' @return sf collection of POLYGON with the following variable
#' \itemize{
#' \item{p1, p2, p3 the node identifiers for the three points define the polygon}
#' \item{geometry 4 point polygon (closed so first and last the same)}
#' }
get_node_mesh_geometry <- function(x, what = 'lonlat'){

  p <- distinct_polygons(x, what = 'nodes')
  m <-as.matrix(p)
  nodes <- fvcom_nodes(x, form = 'table', what = what)
  xy <- nodes %>%
    dplyr::select(-node) %>%
    as.matrix()
  nm <- colnames(xy)
  g <- lapply(seq_len(nrow(p)),
    function(i){
      ix <- c(m[i,], m[i,1])
      sf::st_polygon(list(xy[ix,]))
    })

  p %>%
    dplyr::mutate( geometry = g) %>%
    sf::st_sf(sf_column_name = "geometry", crs = fvcom_crs(what))

}


#' Compute a table of distinct polygons (triangles) by listing the
#' nodes (or elements).
#'
#' @export
#' @param x FVCOM ncdf4 object
#' @param what character, either 'nodes' or "elems"
#' @return ordered tibble of p1, p2, and p3
distinct_polygons <- function(x, what = c("nodes", "elems")[1]){

  if (tolower(what[1]) == "nodes"){
    n   <- ncdf4::ncvar_get(x, "ntsn")
    nbs <- ncdf4::ncvar_get(x, "nbsn")
    nav <- cbind(nbs, n)
    nc <- ncol(nav)
  } else {
    n   <- ncdf4::ncvar_get(x, "ntve")
    nbs <- ncdf4::ncvar_get(x, "nbve")
    nav <- cbind(nbs, n)
    nc <- ncol(nav)
  }

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
  # convert to tibble and arrange by column
  # retain only the unique ones
  colnames(ns) <- paste0("p", 1:3)
  dplyr::as_tibble(ns) %>%
    dplyr::arrange(p1, p2, p3) %>%
    dplyr::distinct_all()
}

#' Retrieve bathymetry "sea_floor_depth_below_geoid" in meters
#' with positive downward.
#'
#' @export
#' @param x ncdf4 object
#' @return something
bathymetry <- function(x,
                       form = c("table", "raster", "mesh")[1]){

  xyz <- dplyr::bind_cols(
    fvcom_nodes(x, what = 'lonlat'),
    dplyr::tibble(bathymetry = ncdf4::ncvar_get(x, "h")))
  if (tolower(form[1]) == "raster"){
    xr <- range(xyz$lon)
    yr <- range(xyz$lat)
    r <- 0.01
    template <- raster(res = r,
                       xmn = xr[1] - r/2,
                       xmx = xr[2] + r/2,
                       ymn = yr[1] - r/2,
                       ymx = yr[2] + r/2,
                       crs = "+init=epsg:4326")
    xyz <- sf::st_as_sf(xyz, coords = c("lon", "lat"),
                        crs = "+init=epsg:4326")
    xyz <- raster::rasterize(xyz, template,
                             field = "bathymetry",
                             fun = mean)

  }

  if (tolower(form[1]) == "mesh"){

  }
  xyz
}