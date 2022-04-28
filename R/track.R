#  Track multiple particles
#  
#  @param X FVCOM_Physics object
#  @param P0 starting points, see \code{FVCOM_Physics$random_point}
#  @param tstep numeric, seconds between each iteration
#  @param tmax numeric, number of seconds to run for
#  @param show_progress logical, if TRUE then show a progress bar
#  @param verbose logical, if TRUE output messages (for development)
#  @param reverse logical, if TRUE run the track reverse in time.  It is an error
#    to provide the seed point with a timestamp equivalent to the first timestep of the
#    model.  See \code{\link{random_point}} to set the seed with other timestamps
#  @param drag numeric, a three element vector of drag factors in [x, y, z], like a sinking rate in z.
#    Drag units must be the same as the units of \code{u}, \code{v}, and \code{ww}.
#  @param resistance numeric, if non zero then apply this to the speed computed 
#    at each each time step.  Negative to impeded progress, positive to add to progress.
#    Zero has no effect
#  @param resistance_type character, one of 'proportional' or 'constant' Proportional is a proportion 
#    of the speed, thus typically a fraction in the range of [-1, 1].  Constant is applied uniformly
#    and is in the unit/time (typically m/s) of the data. 
#  @param fixed_z logical, if TRUE then depths are fixed to the initial depths, and 
#   \code{drag} is truncated to two elements.
#  @param clip_z logical, if TRUE clip Z positions to be between surface and bathymetric depth. 
#  @return sf object of type POINT
.multi_track <- function(X, P0 = X$random_points(n = 2),
                         tstep = 60,
                         tmax = 3600 * 12,
                         reverse = FALSE,
                         drag = c(0,0,0),
                         resistance = c(0,0,0),
                         resistance_type = c("proportional", "constant")[1],
                         fixed_z = FALSE,
                         clip_z = TRUE, 
                         show_progress = FALSE,
                         verbose = FALSE){
  if (FALSE){
    P0 = X$random_points(n = 2)
    tstep = 1200
    tmax = 3600*10
    reverse = FALSE
    drag = c(0,0,0)
    fixed_z = TRUE
    clip_z = TRUE
    show_progress = FALSE
    verbose = TRUE
    resistance = c(0,0,0)
    resistance_type = c("proportional", "constant")[1]
  }
  
  if (fixed_z){
    fixed_depth <- as.vector(sf::st_coordinates(P0)[,'Z'])
    drag <- drag[1:2]
  }
  do_resist <- sum(abs(resistance)) > 0
  
  
  TIMES <- X$get_time() # X$NC$dim$time$vals
  if (reverse && any(P0$time <= TIMES[1])){
    stop("if operating in reverse, the seed point(s) must be later than the first model time slice")
  }
  
  NMAX = tmax/tstep
  NPOINT <- nrow(P0)
  track_iter <- seq_len(NPOINT)
  #preallocate the points list
  PP <- lapply(track_iter, function(i) {vector(length = NMAX, mode = "list")}) 
  # seed the first point
  N <- 1
  # track who continues and who doesn't in each iteration
  OK <- rep(TRUE, NPOINT)
  for (i in track_iter) {
    PP[[i]][[N]] <- P0 |> 
      dplyr::slice(i) |>
      dplyr::mutate(track = i) |>
      dplyr::relocate(.data$track, .before = 1)
  }
  
  if (show_progress[1]){
    pb <- utils::txtProgressBar(min = 0, max = NMAX, initial = 0, style = 3)
  }
  
  if (reverse){
    tstep <- -1 * tstep
  }
  
  # depth ("h") is positive down but ww is "upward velocity"  Argh!
  uvw_sign <- c(1,1,-1)
  
  while (N <= NMAX){
    if (verbose) cat("step N =", N, "\n")
    if (show_progress[1]) utils::setTxtProgressBar(pb, N)
    itime <- sapply(track_iter,
                    function(i){
                      if(OK[i]){
                        findInterval(PP[[i]][[N]]$time[1], TIMES)
                      } else {
                        -1
                      }
                    })
    ok <- itime > 0
    if (any(!ok)){
      newbad <- (ok == FALSE) & (OK == TRUE)
      if (any(newbad)){
        if (verbose) cat("timed out:", which(newbad), "\n")
        OK[!ok] <- FALSE
      }
    }
    if (all(!OK)) break
    
    # get the current point's u,v and w
    uvw <- lapply(track_iter,
                  function(i){
                    if(OK[i]){
                      fvcom::get_elem_var(X$NC, var = c("u", "v", "ww"),
                                          elem = PP[[i]][[N]]$elem,
                                          time = itime[[i]]) |>
                        as.matrix()
                    } else {
                      NULL
                    }
                  })
    # and it's coordinates
    pn <- lapply(track_iter,
                 function(i){
                   if(OK[i]){
                     sf::st_coordinates(PP[[i]][[N]])
                   } else {
                     NULL
                   }
                 })

    
    # translate (aka affine shift)
    dd <- lapply(track_iter,
                 function(i){
                   if(OK[i]){
                     if (fixed_z){
                       r <- c(pn[[i]][1:2] + ((uvw[[i]][,2:3] + drag) * tstep),
                              fixed_depth[i])
                       if (do_resist){
                         if (resistance_type == "constant"){
                           r[1:2] <- r[1:2] + resistance[1:2]
                         } else {
                           r[1:2] <- r[1:2] + (r[1:2] * resistance[1:2])
                         }
                       }
                     } else {
                       r <- pn[[i]] + ((uvw[[i]][,2:4] + drag) * uvw_sign * tstep)
                       if (do_resist){
                         if (resistance_type[1] == "constant"){
                           r <- r + resistance
                         } else {
                           r <- r + (r * resistance)
                         }
                       }
                     }   
                   } else {
                     r <- NULL
                   }
                  r
                  })
    
    # convert to sfc 
    gg <- lapply(track_iter,
                 function(i){
                   if(OK[i]){
                     sf::st_sfc(sf::st_point(dd[[i]]), crs = sf::st_crs(X$M))
                   } else {
                     NULL
                   }
                 })
    
    # determine which element the point belongs to
    ix <- lapply(track_iter,
                 function(i){
                   if (OK[i]){
                     lengths(sf::st_contains(X$M, gg[[i]])) > 0
                   } else {
                     NULL
                   }
                 })
    
    elem <- lapply(track_iter,
                   function(i){
                     if (OK[i]){
                       which(ix[[i]])
                     } else {
                       NULL
                     }
                   })
    
    ok <- lengths(elem) > 0
    if (any(!ok)){
      newbad <- (ok == FALSE) & (OK == TRUE)
      if (any(newbad)){
        if (verbose) cat("track drifted out of bounds:", which(newbad), "\n")
        OK[!ok] <- FALSE
      }
    }
    if (all(!OK)) break
    
    if (clip_z){
      # now we need to check the Z values against surface (0) and bathymetric depth (h)
      for (i in track_iter){
        if (OK[i]) {
          d <- dd[[i]][3]
          reset <- FALSE
          if (d < 0) {
            # whoops - it's a flying fish!
            reset <- TRUE
            d <- 0
          } else if (d > X$M$depth[elem[[i]]]){
            # whoops - it plowed into the seabed
            d <- X$M$depth[elem[[i]]]
            reset <- TRUE
          }
          if (reset){
            if (verbose) cat("clipping Z along track:", i, "\n")
            xyz <- sf::st_coordinates(gg[[i]])
            xyz[3] <- d
            gg[[i]] <- sf::st_sfc(sf::st_point(xyz), crs = sf::st_crs(X$M))
          }
        } #OK
      }
    }
    
    # add a new point
    for (i in track_iter){
      if (OK[i]) {
        PP[[i]][[N+1]] <- sf::st_sf(
          dplyr::tibble(
            track = i,
            elem = elem[[i]], 
            time = PP[[i]][[N]]$time[1] + tstep, 
            geometry = gg[[i]])
          )
      } #OK
    }
    # increment N
    N <- N + 1
    
  } # while
  
  if (show_progress) close(pb)
  
  P <- lapply(track_iter,
              function(i){
                dplyr::bind_rows(PP[[i]])
              })  
  
  P
}


#' Track one or more particles
#' 
#' @export
#' @param X FVCOM_Physics object
#' @param P0 starting point(s), see \code{NE_Physics$random_point}
#' @param tstep numeric, seconds between each iteration
#' @param tmax numeric, number of seconds to run for
#' @param show_progress logical, if TRUE then show a progress bar
#' @param verbose logical, if TRUE output messages (for development)
#' @param filename an optional filename to save the track to or NA to skip
#' @param reverse logical, if TRUE run the track reverse in time.  It is an error
#'   to provide the seed point with a timestamp equivalent to the first timestep of the
#'   model.  See \code{\link{random_point}} to set the seed with other timestamps
#' @param drag numeric, a three element vector of drag factors in [x, y, z], like a sinking rate in z.
#'   Drag units must be the same as the units of \code{u}, \code{v}, and \code{ww}.
#' @param resistance numeric, if non zero then apply this to the speed computed 
#'   at each each time step.  Negative to impeded progress, positive to add to progress.
#'   Zero has no effect
#' @param resistance_type character, one of 'proportional' or 'constant' Proportional is a proportion 
#'   of the speed, thus typically a fraction in the range of [-1, 1].  Constant is applied uniformly
#'   and is in the unit/time (typically m/s) of the data. 
#' @param fixed_z logical, if TRUE then depths are fixed to the initial depths, and 
#'   \code{drag} is truncated to two elements.
#' @param clip_z logical, if TRUE clip Z positions to be between surface and bathymetric depth. 
#' @param overwrite logical, if TRUE allow overwriting of existing files
#' @return sf object of type POINT
particle_track <- function(X, P0 = X$random_points(),
                           tstep = 60,
                           tmax = 3600 * 12,
                           reverse = FALSE,
                           drag = c(0,0,0),
                           resistance = c(0,0,0),
                           resistance_type = c("proportional", "constant")[1],
                           fixed_z = FALSE,
                           clip_z = TRUE, 
                           show_progress = FALSE,
                           verbose = FALSE, 
                           filename = c("particle_track.geojson", NA)[2],
                           overwrite = TRUE){
  
  if (FALSE){
    P0 = X$random_points()
    tstep = 60
    tmax = 600
    reverse = FALSE
    drag = c(0,0,0)
    fixed+z = FALSE
    show_progress = FALSE
    verbose = TRUE
    filename = c("particle_track.geojson", NA)[1]
    resistance = c(0,0,0)
    resistance_type = c("proportional", "constant")[1]
  }
  
  if (clip_z && !("depth" %in% colnames(X$M))) mesh <- X$mesh_depth()
  if (length(resistance) < 3) resistance <- rep(resistance[1],3)
  if (length(drag) < 3) drag <- rep(drag[1],3)
  
  PP <- .multi_track(X, P0,
                     tstep = tstep,
                     tmax = tmax,
                     reverse = reverse,
                     drag = drag,
                     resistance = resistance,
                     resistance_type = resistance_type,
                     fixed_z = fixed_z, 
                     clip_z = clip_z,
                     show_progress = show_progress,
                     verbose = verbose)
  
  P <- dplyr::bind_rows(PP)
  
  #if (nrow(P) > 0){
  #  P <- track_add_z(P) |>
  #    track_add_distance(P) 
  #}
  if (nrow(P) > 0 && !is.na(filename[1])){
    P <- write_track(P, filename[1])
  }
  P
}

#' Extract Z from coordinates and add as a variable for one or more tracks
#' 
#' @export
#' @param x sf POINT object, with a \code{track} column.
#' @return the input sf POINT object with added \code{Z} variable (column)
track_add_z <- function(x){
  xyz <- sf::st_coordinates(x)
  nc <- ncol(x)
  dplyr::mutate(x, Z = xyz[,'Z', drop = TRUE], .before = nc)
}

#' Append the distance between points along a one or more tracks in a data frame
#' 
#' @export
#' @param x sf POINT object, with a \code{track} column.  If the \code{track} column is not 
#'   present one is added.
#' @param by_element logical, defaults to TRUE, see \code{\link[sf]{st_distance}}
#' @param as_vector logical, to return a numeric vector otherwise an \code{units}
#'   object is returned
#' @param cumulative logical, if TRUE retrieve the cumulative distance.  Ignored
#'   if \code{as_vector = FALSE}
#' @param ... other arguments for \code{\link[sf]{st_distance}}
#' @return the input sf POINT object with added \code{path_distance} variable (column)
track_add_distance <- function(x, 
                               by_element = TRUE, 
                               as_vector = TRUE, 
                               cumulative = TRUE,
                               ...){
  
  if (!('track' %in% names(x))){
    x <- dplyr::mutate(track = 1, .before = 1)
  }
  
  do_one <- function(x, by_element = TRUE, as_vector = TRUE, cumulative = TRUE, ...){
    y <- dplyr::bind_rows(dplyr::slice(x, 2:nrow(x)), dplyr::slice(x, 1))
    d <- sf::st_distance(x, y, by_element = by_element, ...)
    if (as_vector){
      d <- as.vector(d)
      d <- c(0, d[-length(d)])
      if (cumulative) d <- cumsum(d)
    }
    dplyr::mutate(x, path_distance = d, .before = dplyr::all_of("geometry"))
  }
  
  dplyr::group_by(x, .data$track) |>
    dplyr::group_map(do_one, .keep = TRUE,
                     by_element = by_element, 
                     as_vector = as_vector, 
                     cumulative = cumulative,
                     ...) |>
    dplyr::bind_rows()
}

#' Write a track to file
#' 
#' @export
#' @param x sf track object
#' @param filename character, the filename to write to
#' @param overwrite logical, if TRUE overwrite existing file
#' @param crs NA or crs-castable object (like 4326). If not NA then
#'   transform to the crs just before writing.  (geojson only accepts 4326-ish)
#' @param ... other arguments for \code{\link[sf]{write_sf}}
#' @return the input object
write_track <- function(x, filename = "track.geojson", overwrite = TRUE, 
                        crs = 4326, ...){
  file_exists_already <- file.exists(filename[1])
  if (file_exists_already && !overwrite){
    warning("file already exists unable to overwrite:", filename[1])
    return(x)
  }
  if (file_exists_already && overwrite){
    unlink(filename[1])
  }
  
  ext <- strsplit(basename(filename), ".", fixed = TRUE)[[1]]
  ext <- paste0(".", ext[length(ext)])
  tmpfile <- tempfile(fileext = ext)
  if (!is.null(crs)) {
    sf::write_sf(sf::st_transform(x, crs), tmpfile)
  } else {  
    sf::write_sf(x, tmpfile, ...)
  }
  file.copy(tmpfile, filename[1], overwrite = overwrite)
  unlink(tmpfile)
  x
}


#' Read a track
#' 
#' @export
#' @param filename the name of the file to read
#' @param crs NA or crs-castable object to transform the data
#' @return sf object
read_track <- function(filename, crs = c(NA, "+init=nad83:1802")[2]){
  x <- sf::read_sf(filename)
  if (!is.na(crs)) x <- sf::st_transform(x, crs)
  x
}