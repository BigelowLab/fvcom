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
#  @return sf object of type POINT
.multi_track <- function(X, P0 = X$random_points(n = 2),
                         tstep = 60,
                         tmax = 3600 * 12,
                         reverse = FALSE,
                         drag = c(0,0,0),
                         show_progress = FALSE,
                         verbose = FALSE){
  if (FALSE){
    P0 = X$random_points(n = 2)
    tstep = 1200
    tmax = 3600*10
    reverse = FALSE
    drag = c(0,0,0)
    show_progress = FALSE
    verbose = TRUE
  }
  
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
  
  while (N <= NMAX){
    if (verbose) cat("N =", N, "\n")
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
                     pn[[i]] + ((uvw[[i]][,2:4] + drag) * tstep)
                   } else {
                     NULL
                   }
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
        if (verbose) cat("drifted out of bounds:", which(newbad), "\n")
        OK[!ok] <- FALSE
      }
    }
    if (all(!OK)) break
    
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
#' @param overwrite logical, if TRUE allow overwriting of existing files
#' @return sf object of type POINT
particle_track <- function(X, P0 = X$random_points(),
                           tstep = 60,
                           tmax = 3600 * 12,
                           reverse = FALSE,
                           drag = c(0,0,0),
                           show_progress = FALSE,
                           verbose = FALSE, 
                           filename = c("particle_track.gpkg", NA)[2],
                           overwrite = TRUE){
  
  if (FALSE){
    P0 = X$random_points()
    tstep = 60
    tmax = 600
    reverse = FALSE
    drag = c(0,0,0)
    show_progress = FALSE
    verbose = TRUE
    filename = c("particle_track.gpkg", NA)[1]
  }
  
  PP <- .multi_track(X, P0,
                     tstep = tstep,
                     tmax = tmax,
                     reverse = reverse,
                     drag = drag,
                     show_progress = show_progress,
                     verbose = verbose)
  
  P <- dplyr::bind_rows(PP)
  
  if (nrow(P) > 0 && !is.na(filename[1])){
    if (file.exists(filename[1]) && overwrite) ok <- file.remove(filename[1])
    tf <- tempfile(fileext = ".gpkg")
    ok <- sf::write_sf(P, tf)
    dummy <- file.copy(tf, filename[1])
    dummy <- file.remove(tf)
  }
  P
}

