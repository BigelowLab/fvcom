#' Retrieve a raster template that covers the provided mesh and aligns with MUR SST pixels
#'
#' @seealso \href{https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1}{MUR SST}
#' @export
#' @param x the mesh geometry
#' @param res a one or two element numeric of the cell resolution.  If one element
#'   then it is used in x and y directions.
#' @param crs character string of the mesh
#' @param bb numeric, 4 element extent [left, right, bottom, top] by default matching MURSST
#' @param value numeric, the value to assign to cells
#' @return a raster layer
mur_template <- function(x,
                         res = c(0.01, 0.01),
                         crs = 4326,
                         bb = c(-75.69, -56.85,  35.27,  46.15),
                         value = NA_real_){
  if (length(res) < 2) res <- c(res, res)
  r2 <- res/2
  
  sf::st_bbox(c(xmin = bb[1] - r2[1],
                ymin = bb[3] - r2[2],
                xmax = bb[2] + r2[1],
                ymax = bb[4] + r2[2]),
              crs = crs) |>
    stars::st_as_stars(dx = res[1],
                       dy = res[2],
                       value = value) 
}

#' Retrieve a raster template that covers the provided mesh
#'
#' @export
#' @param x the mesh geometry
#' @param res a one or two element numeric of the cell resolution.  If one element
#'   then it is used in x and y directions.
#' @param crs character string of the mesh
#' @param value numeric, the value to assign to cells
#' @return a raster layer
default_template <- function(x,
                             res = c(0.01, 0.01),
                             crs = sf::st_crs(x),
                             value = NA_real_){

    if (length(res) < 2) res <- c(res, res)
    r2 <- res/2
    bb <- sf::st_bbox(x)
    sf::st_bbox(c(xmin = bb[["xmin"]] - r2[1],
                  ymin = bb[["ymin"]] - r2[2],
                  xmax = bb[["xmax"]] + r2[1],
                  ymax = bb[["ymax"]] + r2[2]),
                crs = crs) |>
      stars::st_as_stars(dx = res[1],
                         dy = res[2],
                         value = value) 
}

#' Interpolate a raster from a mesh
#'
#' @export
#' @param x the mesh as `sf`` POLYGON
#' @param template a raster template see \code{\link{default_template}} and \code{\link{mur_template}}
#' @param ... further arguments for \code{\link[stars]{st_rasterize}}
#' @return a raster layer
#' @examples
#' \dontrun{
#'   mesh <- fvcom::get_mesh(x, vars = c("u", "v"), what = "lonlat")
#'   uv <- lapply(c("u", "v"), function(f) fvcom::rasterize(mesh[f]))
#'   
#' }
rasterize <- function(x,
                      template = default_template(x),
                      ...){
  stars::st_rasterize(x, template = template, ...)
}
