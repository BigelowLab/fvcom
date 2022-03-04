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
#' @param x sf POINT tibble.  If it has a 'track' variable (column) then each is plotted
#' @param X NE_Physics object
#' @param title character plot title
#' @param filename character or NA, optional output file as PNG
#' @param ext object that defines plot extent, by default \code{p} See \code{\link[sf]{plot}}
#' @param legend_pos character, location of legend, see \code{graphics\link[graphics]{legend}},
#'   set to "none" to not show legend.  Default is "topleft"
#' @param ... other arguments for \code{\link[grDevices]{png}} 
plot_track <- function(x, X = NULL, 
                       title = "Particle Track",
                       filename = c(NA,"particle_track.png")[1],
                       ext = x,
                       legend_pos = c("none", "topleft")[2],
                       ...){
    
  opar <- par(no.readonly = TRUE)
  on.exit({
    par(opar)
  })
  
  legend_pos <- tolower(legend_pos[1])
  
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
       col = "#FFFFFF",
       reset = legend_pos == "none")
  
  if (!is.null(X)){
      plot(sf::st_geometry(X$M),
           border = cols[['gray']],
           add = TRUE)
  }
  
  if (!("track" %in% colnames(x))){
      x <- x |>
          dplyr::mutate(track = 1)
  }
  
  n <- length(unique(x$track))
  colors <- cols[x$track]
  ucols <- cols[seq_len(n)+1]
  x <- x |>
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
              x
          }, .keep = TRUE ) 
  
  legend_pos <- tolower(legend_pos[1])
  if (legend_pos != "none"){
    legend(legend_pos,
           legend = as.character(seq_along(ucols)),
           col = ucols,
           pch = 19,
           lwd = 2,
           bty = "n",
           pt.cex = 2)
  }
  
  if (!is.na(filename[1])){
      ok <- grDevices::dev.off()
  }
  invisible(NULL)
}

#' Plot one or more tracks by depth (Z)
#' 
#' @export
#' @param x sf POINT object with one or more tracks
#' @param title char, title to apply to the plot
#' @param zvar char, the name of the zvariable
#' @param xvar char, the name of the independent variable ("x")
#' @param xlab char,  x axis label
#' @param ylab char, y axis label
#' @param filename NA or char, for optional output
#' @param ... other arguments for the graphics device (\code{\link[grDevices]{pdf}} or \code{\link[grDevices]{png}})
plot_z <- function(x,
                   title = "Particle Track - Z",
                   zvar = c("sigma", "depth", "Z")[3],
                   xvar = c("path_distance", "time")[1],
                   ylab = c(sigma = "Sigma", depth = "Depth (m)", Z = "Depth (m)")[zvar],
                   xlab = c(path_distance = "Path Distance (m)", time = "Time")[xvar],
                   filename = c(NA,"track-Z.png")[1],
                   ...){
  
  zvar <- zvar[[1]]
  xvar <- xvar[[1]]
  if(!(zvar %in% colnames(x))) stop("input must have zvar variable:", zvar)
  if(!(xvar %in% colnames(x))) stop("input must have xvar variable:", xvar)
  
  cols <- grDevices::palette.colors()
  
  if (!('track' %in% colnames(x))){
    x <- dplyr::mutate(track = 1, .before = 1)
  }
  
  xlim <- range(x[[xvar[1]]], na.rm = TRUE)
  ylim <- rev(range(x[[zvar]], na.rm = TRUE))
  
  if (!is.na(filename[1])){
    if (grepl(".pdf", filename[1], fixed = TRUE)){
      grDevices::pdf(filename[1], ...)
    } else {
      grDevices::png(filename[1], ...)
    }
  }
  
  
  plot(xlim, ylim, ylim = ylim, type = "n",
       xlab = xlab, ylab = ylab, 
       main = title)
  x <- x |>
    dplyr::group_by(.data$track) |>
    dplyr::group_map(
      function(x, key){
        i <- as.numeric(as.character(x$track[1]))
        lines(x[[xvar]], x[[zvar]],
              col = cols[i+1],
              type = "l",
              lwd = 2)
        y <- x |> dplyr::slice(c(1,dplyr::n()))
        points(y[[xvar]], y[[zvar]],
               col = cols[i+1],
               pch = c(1, 19),  # open = start, closed = end
               cex = 1.5)
        x
      }, .keep = TRUE ) 
  
  if (!is.na(filename[1])){
    ok <- grDevices::dev.off()
  }
  invisible(NULL)
}


# Plot one or more tracks by depth (Z) with ggplot
# 
# @export
# @param x sf POINT object with one or more tracks
# @param title char, title to apply to the plot
# @param zvar char, the name of the zvariable
# @param xvar char, the name of the independent variable ("x")
# @param ... not sure
# return(ggplot2 object)
# ggplot_z <- function(x,
#                    title = "Particle Track - Z",
#                    zvar = c("sigma", "depth", "Z")[3],
#                    xvar = c("path_distance", "time")[1],
#                    ylab = c(sigma = "Sigma", depth = "Depth (m)", Z = "Depth (m)")[zvar],
#                    xlab = c(path_distance = "Path Distance (m)", time = "Time")[xvar],
#                    #filename = c(NA,"track-Z.png")[1],
#                    ...){
#   
#   zvar <- zvar[[1]]
#   xvar <- xvar[[1]]
#   if(!(zvar %in% colnames(x))) stop("input must have zvar variable:", zvar)
#   if(!(xvar %in% colnames(x))) stop("input must have xvar variable:", xvar)
#   
#   if (!('track' %in% colnames(x))){
#     x <- dplyr::mutate(track = 1, .before = 1)
#   }
#   
#   tracks <- sort(unique(x$track))
#   ntracks <- length(tracks)
#   cols <- grDevices::palette.colors(seq_len(ntracks) + 1)
#   trcak_cols <- cols[x$track]
#   
#   
#   x <- dplyr::mutate(x, 
#                      track = factor(track),
#                      track_col = track_col )
#   
#   xlim <- range(x[[xvar[1]]], na.rm = TRUE)
#   ylim <- rev(range(x[[zvar]], na.rm = TRUE))
#   
#   #if (!is.na(filename[1])){
#   #  if (grepl(".pdf", filename[1], fixed = TRUE)){
#   #    grDevices::pdf(filename[1], ...)
#   #  } else {
#   #    grDevices::png(filename[1], ...)
#   #  }
#   #}
#   x <- sf::st_drop_geometry(x) |>
#     dplyr::group_by(.data$track)
#   
#   startstop <- function(x, y){
#     dplyr::slice(x, c(1, dplyr::n()))
#   }
#   ends <- x |>
#     group_map(startstop, .keep = TRUE) |>
#     dplyr::bind_rows() |>
#     dplyr::group_by(.data$track)
#   
#   ggplot2::ggplot(data = x,
#                   ggplot2::aes_string(x = xvar, y = zvar)) +
#     ggplot2::scale_color_discrete(type = unname(cols)) + 
#     ggplot2::geom_line(size = 1, ggplot2::aes(color = .data$track_col)) +
#     ggplot2::geom_point(data = ends |> dplyr::slice(1),
#                         ggplot2::aes_string(x = xvar, y = zvar),
#                         shape = 1, size = 1.5,
#                         color = .data$track_col) + 
#     ggplot2::geom_point(data = ends |> dplyr::slice(dplyr::n()),
#                         ggplot2::aes_string(x = xvar, y = zvar),
#                         shape = 19, size = 1.5) +
#     ggplot2::facet_wrap(dplyr::all_of("track"), ncol = 1,
#                         scales = "free_y")
#     
#   ?plot(xlim, ylim, ylim = ylim, type = "n",
#        xlab = xlab, ylab = ylab, 
#        main = title)
#   x <- x |>
#     dplyr::group_by(.data$track) |>
#     dplyr::group_map(
#       function(x, key){
#         i <- as.numeric(as.character(x$track[1]))
#         lines(x[[xvar]], x[[zvar]],
#               col = cols[i+1],
#               type = "l",
#               lwd = 2)
#         y <- x |> dplyr::slice(c(1,dplyr::n()))
#         points(y[[xvar]], y[[zvar]],
#                col = cols[i+1],
#                pch = c(1, 19),  # open = start, closed = end
#                cex = 1.5)
#         x
#       }, .keep = TRUE ) 
#   
#  gg
# }