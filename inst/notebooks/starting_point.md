starting point
================

### Question: how do I use my own point to start a track?

I dropped a float with a GPS on it. I picked it up hours later. How can
I see what the model would have predicted for the float?

My starting data…

``` r
suppressPackageStartupMessages({
  library(cascophys)
  library(fvcom)
  library(sf)
  library(dplyr)
})

# takes a while!
CB <- CascoBayPhysics(verbose = TRUE)
```

    ## opening NCDF4
    ## retrieving mesh

``` r
mystart <- dplyr::tibble(
  lon = -70.127978,      # <- insert your own
  lat = 43.785747,       # <- insert your own
  z = 0,
  time = CB$get_time()[1], # <- sort has to be faked unless you did this in 2018...
  ) |>
  sf::st_as_sf(coords = c("lon", "lat", "z"), crs = 4326) |>  # make it geospatial
  sf::st_transform(crs = sf::st_crs(CB$M)) # and convert to the projection the mesh uses
```

It’s a messy projection description… no handy shorthand 4 digit code
here like 4326.

``` r
mystart
```

    ## Simple feature collection with 1 feature and 1 field
    ## Geometry type: POINT
    ## Dimension:     XYZ
    ## Bounding box:  xmin: 903114.1 ymin: 105809.2 xmax: 903114.1 ymax: 105809.2
    ## z_range:       zmin: 0 zmax: 0
    ## CRS:           PROJCS["unknown",GEOGCS["unknown",DATUM["North_American_Datum_1983",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],AUTHORITY["EPSG","6269"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",42.8333333333333],PARAMETER["central_meridian",-70.1666666666667],PARAMETER["scale_factor",0.999966666666667],PARAMETER["false_easting",900000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
    ## # A tibble: 1 × 2
    ##   time                               geometry
    ## * <dttm>                          <POINT [m]>
    ## 1 2018-05-04 00:00:00 Z (903114.1 105809.2 0)
