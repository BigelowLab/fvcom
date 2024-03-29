#' Retrieve a raster template that covers the provided mesh and aligns with MUR SST pixels
#'
#' @seealso \href{https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1}{MUR SST}
#' @export
#' @param x the mesh geometry
#' @param res a one or two element numeric of the cell resolution.  If one element
#'   then it is used in x and y directions.
#' @param crs character string of the mesh
#' @param bb numeric, 4 element extent [left, right, bottom, top] by default matching MURSST
#' @param ... further arguments for \code{\link[raster]{raster}}
#' @return a raster layer
mur_template <- function(x,
                         res = c(0.01, 0.01),
                         crs = "epsg:4326",
                         bb = c(-75.69, -56.85,  35.27,  46.15),
                         ...){
  if (length(res) < 2) res <- c(res, res)
  r2 <- res/2
  raster::raster(
    res = res,
    xmn = bb[1] - r2[1],
    xmx = bb[2] + r2[1],
    ymn = bb[3] - r2[2],
    ymx = bb[4] + r2[2],
    crs = crs,
    ...)
}

#' Retrieve a raster template that covers the provided mesh
#'
#' @export
#' @param x the mesh geometry
#' @param res a one or two element numeric of the cell resolution.  If one element
#'   then it is used in x and y directions.
#' @param crs character string of the mesh
#' @param ... further arguments for \code{\link[raster]{raster}}
#' @return a raster layer
default_template <- function(x,
                             res = c(0.01, 0.01),
                             crs = sf::st_crs(x)[['epsg']],
                             ...){

    if (length(res) < 2) res <- c(res, res)
    d2 <- res/2
    bb <- as.vector(sf::st_bbox(x))
    raster::raster(
      res = res,
      xmn = bb[1] - d2[1],
      xmx = bb[3] + d2[1],
      ymn = bb[2] - d2[2],
      ymx = bb[4] + d2[2],
      crs = crs,
      ...)
}

#' Interpolate a raster from a mesh
#'
#' @export
#' @param x the mesh as `sf`` POLYGON
#' @param template a raster template see \code{\link{default_template}} and \code{\link{mur_template}}
#' @param ... further arguments for \code{\link[fasterize]{fasterize}}
#' @return a raster layer
#' @examples
#' \dontrun{
#'   mesh <- fvcom::get_mesh(x, vars = c("u", "v"), what = "lonlat")
#'   uv <- raster::stack(sapply(c("u", "v"), function(f) rasterize(mesh, field = f), simplify = FALSE))
#'   rasterVis::vectorplot(uv, isField = TRUE, main = 'Surface Currents')
#' }
rasterize <- function(x,
                      template = default_template(x),
                      ...){
  fasterize::fasterize(x, template, ...)
}
