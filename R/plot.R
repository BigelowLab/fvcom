#' Plot element mesh - simplified
#' 
#' @seealso \code{\link{plot_mesh_geometry}}
#' @export
#' @param x the FVCOM_Physics object
#' @param border character color for drawing outline
#' @param ... other arguments for plot.sfc
plot_mesh <- function(x,
                      border = "#999999",
                      ...){
    plot_mesh_geometry(x$M,
         border = border,
         label_vertices = FALSE,
         label_polygons = FALSE,
         ...)
}


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
    plot(sf::st_geometry(mesh), ...)
    if (label_polygons){

        cg <- suppressWarnings(sf::st_centroid(mesh)) |>
            sf::st_coordinates()
        text(cg[,"X"], cg[,"Y"], labels = seq_len(nrow(mesh)), cex = 1.2)
    }

    if (label_vertices){
        for (i in seq_len(nrow(mesh))){
            m <- mesh |> dplyr::slice(i)
            v <- sf::st_coordinates(m)
            n <- seq_len(nrow(v)-1)
            text(v[n,"X"], v[n,"Y"], labels = c(m$p1, m$p2, m$p3), adj = c(1,1))
        }
    }

}

#' Plot a track or series of tracks
#' 
#' @export
#' @param p sf POINT tibble.  If it has a 'track' variable (column) then each is plotted
#' @param X NE_Physics object
#' @param title character plot title
#' @param filename character or NA, optional output file as PNG
#' @param ext object that defines plot extent, by default \code{p} See \code{\link[sf]{plot}}
#' @param ... other arguments for \code{\link[grDevices]{png}} 
plot_track <- function(p, X = NULL, 
                       title = "Particle Track",
                       filename = c(NA,"particle_track.png")[1],
                       ext = p,
                       ...){
    
    cols <- grDevices::palette.colors()
    if (!is.na(filename[1])){
        grDevices::png(filename[1], ...)
    }
    
    plot(sf::st_geometry(ext),
         xlab = 'Easting (m)', 
         ylab  = 'Northing (m)',
         axes = TRUE,
         main = title,
         extent = ext,
         col = "#FFFFFF")
    
    if (!is.null(X)){
        plot(sf::st_geometry(X$M),
             border = cols[['gray']],
             add = TRUE)
    }
    
    if (!("track" %in% colnames(p))){
        p <- p |>
            dplyr::mutate(track = 1)
    }
    
    n <- length(unique(p$track))
    colors <- cols[p$track]
    p <- p |>
        dplyr::group_by(.data$track) |>
        dplyr::group_map(
            function(x, key){
                i <- x$track[1]
                plot(sf::st_geometry(x),
                     col = cols[i+1],
                     pch = ".",
                     type = "l",
                     lwd = 2,
                     add = TRUE)
                plot(sf::st_geometry(x |> dplyr::slice(c(1,dplyr::n()))),
                     col = cols[i+1],
                     pch = c(1, 19),  # open = start, closed = end
                     cex = 1.5,
                     add = TRUE)
                p
            }, .keep = TRUE ) 
    
    
    if (!is.na(filename[1])){
        ok <- grDevices::dev.off()
    }
    invisible(NULL)
}


