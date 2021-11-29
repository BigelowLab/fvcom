Rare proj strings to WKT
================

### Background

[CRS](https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf)
(aka coordinate reference system) is how spatial data knows what
projection it is in (if any). For a long time
[rgdal](https://CRAN.R-project.org/package=rgdal) has been the
intermediary between the many CRSs in the world and R. It has primarily
supported [Proj strings](https://proj.org/) to negotiate data to-from R
and [GDAL](https://gdal.org/).

Changes are afoot. Soon (2023) rgdal maintenance will end in favor of
direct interaction between R and GDAL. Soon the necessary
coding/decoding will be handled within the
[sf](https://CRAN.R-project.org/package=sf) package using
[EPSG-like](https://epsg.io/) codes or [Well Known
Text](https://en.wikipedia.org/wiki/Well-known_text_representation_of_coordinate_reference_systems#ESRI_vs_OGC)
aka WKT.

### Transitions when it is easy

Many datasets come with simple log/lat values - that is there is no
projection. In this case the easiest CRS is `EPSG:4326` or `WGS84`. The
following are all equivalent.

``` r
library(sf)
```

    ## Linking to GEOS 3.7.2, GDAL 3.0.4, PROJ 6.3.2

``` r
crs_a <- sf::st_crs("EPSG:4326")
crs_b <- sf::st_crs(4326)
crs_c <- sf::st_crs("WGS84")
(crs_a == crs_b) && (crs_a == crs_c)
```

    ## [1] TRUE

### Transitions when it is hard

In the meantime, there is a lot of data served that is projected and
that retains to use of Proj strings rather than an EPSG code or WKT.
Below is an example from a NetCDF file of a ocean dynamics model. While
we still have access to rdgal package we can leverage it to help convert
frpm proj strings to WKT or possibly EPSG (for those stored that might
be in your database of CRSs.)

``` r
library(rgdal)
```

    ## Loading required package: sp

    ## Please note that rgdal will be retired by the end of 2023,
    ## plan transition to sf/stars/terra functions using GDAL and PROJ
    ## at your earliest convenience.
    ## 
    ## rgdal: version: 1.5-26, (SVN revision 1148)
    ## Geospatial Data Abstraction Library extensions to R successfully loaded
    ## Loaded GDAL runtime: GDAL 3.0.4, released 2020/01/28
    ## Path to GDAL shared files: /usr/share/gdal
    ## GDAL binary built with GEOS: TRUE 
    ## Loaded PROJ runtime: Rel. 6.3.2, May 1st, 2020, [PJ_VERSION: 632]
    ## Path to PROJ shared files: /usr/share/proj
    ## Linking to sp version:1.4-5
    ## To mute warnings of possible GDAL/OSR exportToProj4() degradation,
    ## use options("rgdal_show_exportToProj4_warnings"="none") before loading sp or rgdal.

``` r
proj_string <- "proj=tmerc +datum=NAD83 +lon_0=-70d10 lat_0=42d50 k=.9999666666666667 x_0=900000 y_0=0"
```

Here we use rgdal to cast the proj\_string to WKT

``` r
wkt <- rgdal::showWKT(proj_string)
wkt
```

    ## [1] "PROJCS[\"unknown\",GEOGCS[\"unknown\",DATUM[\"North_American_Datum_1983\",SPHEROID[\"GRS 1980\",6378137,298.257222101,AUTHORITY[\"EPSG\",\"7019\"]],AUTHORITY[\"EPSG\",\"6269\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"latitude_of_origin\",42.8333333333333],PARAMETER[\"central_meridian\",-70.1666666666667],PARAMETER[\"scale_factor\",0.999966666666667],PARAMETER[\"false_easting\",900000],PARAMETER[\"false_northing\",0],UNIT[\"metre\",1,AUTHORITY[\"EPSG\",\"9001\"]],AXIS[\"Easting\",EAST],AXIS[\"Northing\",NORTH]]"

Now we can pass that to `sf::st_crs()` to use within sf package.

``` r
crs <- sf::st_crs(wkt)
crs
```

    ## Coordinate Reference System:
    ##   User input: PROJCS["unknown",GEOGCS["unknown",DATUM["North_American_Datum_1983",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],AUTHORITY["EPSG","6269"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",42.8333333333333],PARAMETER["central_meridian",-70.1666666666667],PARAMETER["scale_factor",0.999966666666667],PARAMETER["false_easting",900000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]] 
    ##   wkt:
    ## PROJCRS["unknown",
    ##     BASEGEOGCRS["unknown",
    ##         DATUM["North American Datum 1983",
    ##             ELLIPSOID["GRS 1980",6378137,298.257222101,
    ##                 LENGTHUNIT["metre",1]],
    ##             ID["EPSG",6269]],
    ##         PRIMEM["Greenwich",0,
    ##             ANGLEUNIT["degree",0.0174532925199433],
    ##             ID["EPSG",8901]]],
    ##     CONVERSION["unnamed",
    ##         METHOD["Transverse Mercator",
    ##             ID["EPSG",9807]],
    ##         PARAMETER["Latitude of natural origin",42.8333333333333,
    ##             ANGLEUNIT["degree",0.0174532925199433],
    ##             ID["EPSG",8801]],
    ##         PARAMETER["Longitude of natural origin",-70.1666666666667,
    ##             ANGLEUNIT["degree",0.0174532925199433],
    ##             ID["EPSG",8802]],
    ##         PARAMETER["Scale factor at natural origin",0.999966666666667,
    ##             SCALEUNIT["unity",1],
    ##             ID["EPSG",8805]],
    ##         PARAMETER["False easting",900000,
    ##             LENGTHUNIT["metre",1],
    ##             ID["EPSG",8806]],
    ##         PARAMETER["False northing",0,
    ##             LENGTHUNIT["metre",1],
    ##             ID["EPSG",8807]]],
    ##     CS[Cartesian,2],
    ##         AXIS["easting",east,
    ##             ORDER[1],
    ##             LENGTHUNIT["metre",1,
    ##                 ID["EPSG",9001]]],
    ##         AXIS["northing",north,
    ##             ORDER[2],
    ##             LENGTHUNIT["metre",1,
    ##                 ID["EPSG",9001]]]]

### What happens when rgdal is no longer supported?

Not sure. Possibly
[gdalUtilities](https://github.com/JoshOBrien/gdalUtilities) or
[gdalUtils](https://github.com/JoshOBrien/gdalUtils)?
