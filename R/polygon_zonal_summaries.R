# Processing FVCOmTimeseries for a Given Area:
# First four functions feed into full workflow
# 1. get the intersection
# 2. obtain the element-wise average based on node values
# 3. ^ repeat the above for all elements
# 4. Average across elements weighted by area






#' Perform polygon intersection with FVCOM Mesh
#' 
#' @description
#' Return the geometric intersection of a spatial polygon and the FVCOM 
#' unstructured mesh.
#' 
#' Returns the mesh elements and their nodes that fall within the area of 
#' interest. 
#' 
#' Optionally returns the remaining relative areas for all elements that fill
#' the interior space of the shapefile. This step gathers necessary information 
#' for performing a longitudinal (across time) zonal summary that weights values
#' by area.
#' 
#'
#' @param poly_sf sf object of polygon to pass to sf::st_intersection.
#' @param fvcom_mesh sf object with FVCOM mesh geometry and the variables.
#' @param relative_areas Boolean T/F to perform area calculation on resulting elements.
#' 
#'
#' @return
#' @export
#'
#' @examples
get_poly_mesh_intersection <- function(poly_sf, fvcom_mesh, relative_areas = TRUE){
  
  # Add a crs check and st_transform step/error
  
  # Run intersection
  mesh_clip <- sf::st_intersection(x = fvcom_mesh, y = poly_sf) 
  if(relative_areas == FALSE){
    return(sf::st_drop_geometry(mesh_clip))}
  
  # Get relative area of triangle areas for weighting
  mesh_intersection_df <- mesh_clip %>% 
    dplyr::mutate(rel_area = sf::st_area(mesh_clip)) %>% 
    sf::st_drop_geometry()
  
  # Return
  return(mesh_intersection_df)
  
}



#' Get the Average Value for one FVCOM mesh Element using its Nodes
#' 
#' 
#' @description
#' Takes a dataframe with element and node information [elem, p1, p2, p3], the 
#' ID for the element of interest, and the name of the netcdf variable 
#' and returns the average value of the three nodes for that triangle element.
#' 
#'
#' @param fvcom_mesh_df A dataframe containing the element and node trios to use
#' when looking up the relevant nodes for each element.
#' @param fvcom_nc Open FVCOM file connection from ncsf4::nc_open
#' @param elem_id Integer values for the element index numbers for the FVCOM 
#' mesh.
#' @param nc_varname String indicating FVCOM variable of interest
#' @param siglev Sigma level index value. Sigma levels are depth layers in 
#' FVCOM. Accepts integer values from 1 (surface) to 45 (bottom).
#'
#' @return
#' @export
#'
#' @examples
get_average_from_nodes <- function(
    zonal_assignments, 
    fvcom_nc, 
    elem_id, 
    nc_varname, 
    siglev = 1){
  
  # Slice the row for that element, get distinct in case >1
  elem_df <- dplyr::filter(
    .data = zonal_assignments, 
    elem == elem_id) %>% 
    dplyr::distinct(elem, p1, p2, p3)
  
  # Pull the element id as a single value
  elem_num <- elem_df[[1, "elem"]]
  
  # Use this when there is no siglev dimension
  # How it would change with siglev:
  # start = c(elem_df[[1, "p1"]], 1),
  # count = c(1, -1))
  
  # Pull vectors (full time dimension) for each point:
  p1_ts <- ncdf4::ncvar_get(
    nc = fvcom_nc, 
    varid = nc_varname, 
    start = c(elem_df[[1, "p1"]], siglev, 1),
    count = c(1, siglev, -1))
  p2_ts <- ncvar_get(
    nc = fvcom_nc, 
    varid = nc_varname, 
    start = c(elem_df[[1, "p2"]], siglev, 1),
    count = c(1, siglev, -1))
  p3_ts <- ncvar_get(
    nc = fvcom_nc, 
    varid = nc_varname, 
    start = c(elem_df[[1, "p3"]], siglev, 1),
    count = c(1, siglev, -1))
  
  # Get Averages, store as named list, name from elem #
  elem_avg_var <- (p1_ts + p2_ts + p3_ts) / 3
  # elem_avg_var <- list(elem_avg_var)
  # elem_avg_var <- setNames(elem_avg_var, elem_id)
  return(elem_avg_var)
  
}




#' Return All Element-Wise Timeseries for table of FVCOM-Mesh Elements
#' 
#' 
#' @description Get the average value from the relevant vertices, along  
#' the time dimension for FVCOM mesh triangles for a table of containing
#' element and node trios.
#' 
#'
#' @param mesh_intersection_df Dataframe containing columns for element and 
#' node ID numbers for the area of interest [elem, p1, p2, p3] 
#' @param fvcom_nc Open FVCOM file connection from ncsf4::nc_open
#' @param nc_varname String indicating FVCOM variable of interest
#' @param siglev Sigma level index value. Sigma levels are depth layers in 
#' FVCOM. Accepts integer values from 1 (surface) to 45 (bottom).
#'
#' @return
#' @export
#'
#' @examples
get_mesh_element_timeseries <- function(
    mesh_intersection_df, 
    fvcom_nc, 
    nc_varname = "surface_t",
    siglev = 1){
  
  
  # Take the unique elements from our table:
  unique_elems <- dplyr::distinct(mesh_intersection_df, elem) %>% 
    dplyr::pull(elem)
  
  # Make a named list for purrr::map()
  unique_elems <- stats::setNames(unique_elems, unique_elems)
  
  # Iterate through each unique element (triangle element, p1, p2, p3)
  # Average the nodes to get value for each element
  elem_average_timeseries <- purrr::map(
    .x = unique_elems, 
    .f = ~get_average_from_nodes(
      zonal_assignments = mesh_intersection_df, 
      fvcom_nc = fvcom_nc, 
      elem_id = .x, 
      nc_varname = nc_varname, 
      siglev = siglev))
  
  return(elem_average_timeseries)
  
}





#' @title Zonal Averages from FVCOM-Polygon Intersection
#' 
#' @description
#'
#' @param mesh_poly_intersection Dataframe with relevant elements & nodes that
#' fell within polygon of interest. Should contain additional column [rel_area] 
#' with relative areas for each element.
#' @param zonal_means Named list with timeseries for each element.
#' @param fvcom_nc pen FVCOM file connection from ncsf4::nc_open
#' @param nc_timedim String indicating time dimension labels to extract as 
#' labels with weighted timeseries.
#'
#' @return
#' @export
#'
#' @examples
get_area_weighted_zonal_average <- function(
    mesh_poly_intersection, 
    zonal_means, 
    fvcom_nc,
    nc_timedim = "Times"){
  
  # Get the elements
  poly_elems <- as.character(mesh_poly_intersection$elem)
  
  # pull out the weights, name them
  poly_wts <- as.numeric(mesh_poly_intersection$rel_area) %>% 
    stats::setNames(poly_elems)
  
  # Weight each timeseries by area, then sum them all
  # Get the timeseries that go with the triangles
  # multiply the relevant relative areas
  poly_wtd_ts <- purrr::map(poly_elems, function(x){
    wtd_vals <- zonal_means[[x]] * poly_wts[[x]]
    return(wtd_vals)}) %>% 
    reduce(.x = ., 
           .f = `+`)
  
  # Divide by total area
  poly_tot_area <- sum(as.numeric(mesh_poly_intersection$rel_area))
  
  # Add the time dimension and return as dataframe
  poly_zonal <- data.frame(
    "time"     = ncdf4::ncvar_get(nc = fvcom_nc, varid = nc_timedim),
    "zonal_mu" = poly_wtd_ts / poly_tot_area)
  return(poly_zonal)
  
}














#' Area-Weighted Timeseries for FVCOM Polygon Intersection Area
#' 
#' 
#' @description
#' A function to return a single timeseries for one variable from an FVCOM .nc 
#' file. Timeseries returned is the area-weighted average of the mesh areas 
#' that fill the polygon space.
#' 
#'
#' @param poly_sf sf object of polygon to pass to sf::st_intersection.
#' @param fvcom_nc 
#' @param fvcom_mesh 
#' @param nc_varname 
#' @param nc_timedim 
#'
#' @return
#' @export
#'
#' @examples
get_timeseries_for_poly <- function(
    poly_sf, 
    fvcom_nc, 
    fvcom_mesh, 
    nc_varname = "temp",
    nc_timedim){
  
  # Do the Intersection
  poly_intersection <- get_poly_mesh_intersection(
    poly_sf = poly_sf,
    fvcom_mesh = fvcom_mesh,
    relative_areas = T)
  
  # Get timeseries for the elements in poly
  poly_elem_timeseries <- get_mesh_element_timeseries(
    mesh_intersection_df = poly_intersection, 
    fvcom_nc = fvcom_nc, 
    nc_varname = nc_varname,
    siglev = 1)
  
  # Get the area-weighted timeseries for the whole poly
  area_wtd_poly_timeseries <- get_area_weighted_zonal_average(
    mesh_poly_intersection = poly_intersection, 
    zonal_means = poly_elem_timeseries, 
    fvcom_nc = fvcom_nc, 
    nc_timedim = nc_timedim)
  
  # Return it
  return(area_wtd_poly_timeseries)
  
  
}





# #### Function TESTING  ####
# 
# # For testing
# library(tidyverse)
# library(sf)
# library(fvcom)
# library(ncdf4)
# 
# # URL to a file - needs to have coordinate values! Some are empty
# test_directory <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/"
# test_file <- "gom3_197801.nc"
# test_nc <- nc_open(str_c(test_directory, test_file))
# 
# 
# # test polygon - from ecodata package
# test_poly <- read_sf(str_c(gmRi::cs_path("res", "Shapefiles/EPU/individual_epus"), "GB.geojson"))
# 
# 
# # Step through functions
# 
# # Get the mesh, do this one time*
# fvcom_mesh <- get_mesh_geometry(x = test_nc)
# 
# # Make sure crs matches
# test_poly <- st_transform(
#   st_union(test_poly), 
#   st_crs(fvcom_mesh))
# 
# # Step 1: Get the intersection table
# gb_intersection <- get_poly_mesh_intersection(
#   poly_sf = test_poly, 
#   fvcom_mesh = fvcom_mesh, 
#   relative_areas = T)
# 
# # Step 2: Get the timeseries for the elements
# single_elem_test <- get_average_from_nodes(
#   zonal_assignments = gb_intersection, 
#   fvcom_nc = test_nc,
#   elem_id = 6429, 
#   nc_varname = "temp", 
#   siglev = 1)
# 
# # Step 2: Do it for all of the elements - loops over distinct "elem"
# all_element_test <- get_mesh_element_timeseries(
#   mesh_intersection_df = gb_intersection, 
#   fvcom_nc = test_nc, 
#   nc_varname = "temp", 
#   siglev = 1)
# # Check one
# all_element_test[1]
# 
# # Step 3: Area weighted average
# area_weight_test <- get_area_weighted_zonal_average(
#   mesh_poly_intersection = gb_intersection, 
#   zonal_means = all_element_test, 
#   fvcom_nc = test_nc, 
#   nc_timedim = "Times")
# 
# # Check it
# area_weight_test
# 
# # Steps 1-3 Together
# gb_ts <- get_timeseries_for_poly(
#   poly_sf = test_poly, 
#   fvcom_nc = test_nc, 
#   fvcom_mesh = fvcom_mesh, 
#   nc_varname = "temp", 
#   nc_timedim = "Times")
# 
# # What are the hourly times?
# names(test_nc$var)
# gb_ts$time <- ncvar_get(test_nc, varid = "Times") %>% 
#   str_remove("T") %>% 
#   base::as.POSIXct()
# ggplot(gb_ts, aes(time, zonal_mu)) +
#   geom_line()
