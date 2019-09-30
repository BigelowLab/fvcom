#' Given a (small) mesh, plot the mesh and possibly label
#'
#' @export
#' @param mesh a mesh produced by \code{\link{get_node_mesh_geometry}}
#' @param label_vertices logical, if TRUE then label the vertices
#' @param label_polygons logical, if TRUE label each polygon with it's row number
#' @param ... further arguments for \code{plot}
plot_mesh_geometry <- function(mesh,
                               ...,
                               label_vertices = TRUE,
                               label_polygons = FALSE){
    plot(st_geometry(mesh), ...)
    if (label_polygons){

        cg <- suppressWarnings(sf::st_centroid(mesh)) %>%
            sf::st_coordinates()
        text(cg[,"X"], cg[,"Y"], labels = seq_len(nrow(mesh)), cex = 1.2)
    }

    if (label_vertices){
        for (i in seq_len(nrow(mesh))){
            m <- mesh %>% dplyr::slice(i)
            v <- sf::st_coordinates(m)
            n <- seq_len(nrow(v)-1)
            text(v[n,"X"], v[n,"Y"], labels = c(m$p1, m$p2, m$p3), adj = c(1,1))
        }
    }

}
