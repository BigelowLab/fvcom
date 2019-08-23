# fvcom

Convenience accessing FVCOM datasets.

### Background

[FVCOM](http://fvcom.smast.umassd.edu/fvcom/) produces ocean circulation models
on an irregular mesh. [Here](http://fvcom.smast.umassd.edu/wp-content/uploads/2013/11/MITSG_12-25.pdf)
is the user manual which looks like a good cure for being bored!

As an example, [NECOFS](http://fvcom.smast.umassd.edu/necofs/) leverages this mesh
to deliver models for the Gulf of Maine, Mass coastal waters and a small number
of estuaries.  NECOFS provides an [interactive web map](http://134.88.228.119:8080/fvcomwms/)


#### Gulf of Maine centric products

There are a number of models with a Gulf of Maine bend including
[forecasts](http://www.smast.umassd.edu:8080/thredds/forecasts.html) and
[archives](http://www.smast.umassd.edu:8080/thredds/catalog/models/fvcom/NECOFS/Archive/NECOFS_GOM/catalog.html)


### Requirements

+ [ncdf4](https://cran.r-project.org/package=ncfd4)


### Installation

```
devtools::install_github("BigelowLab/fvcom")
````


## Data Access

Data are served via OPeNDAP on a [THREDDS server](http://www.smast.umassd.edu:8080/thredds/catalog.html)
THREDDS servers blend a simple user-navigable interface (similar to a file browser)
with a machine readable data [XML map](http://www.smast.umassd.edu:8080/thredds/catalog.xml)
Note the change in the file extension.  Files served this way permit the user to
treat the product as if it were a local file, allowing selective downlaod and
access in a familar NetCDF format with needing to downlaod the entire file.

NECOFS has it's own [THREDDS directory](http://www.smast.umassd.edu:8080/thredds/catalog/models/fvcom/NECOFS/catalog.html)
including a subdirectory for the GoM FVCOM models.


## Accessing FVCOM data

Open the FVCOM resource as you would any NetCDF file.

```{r}
library(fvcom)
library(ncdf4)
uri_base <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/NECOFS_GOM/2019/"
uri <- file.path(uri_base, "gom4_201901.nc")
x <- nc_open(uri)
x
#      ...blah...
#      53 variables (excluding dimension variables):
#         ...blah...
#         float lon[node]   
#             long_name: nodal longitude
#             standard_name: longitude
#             units: degrees_east
#         float lat[node]   
#             long_name: nodal latitude
#             standard_name: latitude
#             units: degrees_north
#         float xc[nele]   
#             long_name: zonal x-coordinate
#             units: meters
#         float yc[nele]   
#             long_name: zonal y-coordinate
#             units: meters
#         ...blah...
```

### Nodes and Elements

The mesh is defined by non-intersecting triangular elements bounded by three nodes. Nodes are
shown below as the solid dots, three nodes define the boundary of element.  Scalar values,
like temperature, salinty and height are defined at nodes.  Vector values, such as velocity are defined
at the element centroids.  Within the NetCDF object you can make complete identification by examining
which variables use the `node` dimensions versus those that use the `nele` dimension.

![mesh-image](inst/nodes-elements.png)

### Node and Element locations

Use the functions `fvcom_nodes` and `fvcom_elems` to extract location information
as either `xy` or `lonlat` pairs.  Note we get both xy and lonlat here for nodes.  The
same can be had for elements. 

```
dplyr::left_join(fvcom::fvcom_nodes(x, what = 'lonlat'), 
                 fvcom::fvcom_nodes(x, what = 'xy'), by = "node")
# Joining, by = "node"
# # A tibble: 53,087 x 5
#     node   lon   lat       x       y
#    <int> <dbl> <dbl>   <dbl>   <dbl>
#  1     1 -59.8  46.1 1699880 420500 
#  2     2 -59.8  46.1 1705180 414890 
#  3     3 -59.7  46.0 1709910 409690 
#  4     4 -59.6  46.0 1715778 403368.
#  5     5 -59.6  45.9 1722603 395907 
#  6     6 -59.5  45.8 1730039 387611.
#  7     7 -59.4  45.7 1737004 380188.
#  8     8 -59.3  45.7 1744943 371707.
#  9     9 -59.2  45.6 1752036 363693 
# 10    10 -59.1  45.5 1761284 353679.
# # … with 53,077 more rows
```

### Timestamps

Retrieving the timestamps provides you with a choice for assumptions you want
to make about the hourly model output.  Timestamps stored internally do not land
evenly on each hour.

```
head(fvcom::dim_times(x, internal = TRUE), n = 24)
#  [1] "2019-01-01 00:00:00 UTC" "2019-01-01 01:01:52 UTC" "2019-01-01 01:58:07 UTC" "2019-01-01 03:00:00 UTC"
#  [5] "2019-01-01 04:01:52 UTC" "2019-01-01 04:58:07 UTC" "2019-01-01 06:00:00 UTC" "2019-01-01 07:01:52 UTC"
#  [9] "2019-01-01 07:58:07 UTC" "2019-01-01 09:00:00 UTC" "2019-01-01 10:01:52 UTC" "2019-01-01 10:58:07 UTC"
# [13] "2019-01-01 12:00:00 UTC" "2019-01-01 13:01:52 UTC" "2019-01-01 13:58:07 UTC" "2019-01-01 15:00:00 UTC"
# [17] "2019-01-01 16:01:52 UTC" "2019-01-01 16:58:07 UTC" "2019-01-01 18:00:00 UTC" "2019-01-01 19:01:52 UTC"
# [21] "2019-01-01 19:58:07 UTC" "2019-01-01 21:00:00 UTC" "2019-01-01 22:01:52 UTC" "2019-01-01 22:58:07 UTC"
```

We can get a proxy for these times, but prettily settled on each hour. The choice is yours.

```
head(fvcom::dim_times(x, internal = TRUE), n = 24)
#  [1] "2019-01-01 00:00:00 UTC" "2019-01-01 01:00:00 UTC" "2019-01-01 02:00:00 UTC" "2019-01-01 03:00:00 UTC"
#  [5] "2019-01-01 04:00:00 UTC" "2019-01-01 05:00:00 UTC" "2019-01-01 06:00:00 UTC" "2019-01-01 07:00:00 UTC"
#  [9] "2019-01-01 08:00:00 UTC" "2019-01-01 09:00:00 UTC" "2019-01-01 10:00:00 UTC" "2019-01-01 11:00:00 UTC"
# [13] "2019-01-01 12:00:00 UTC" "2019-01-01 13:00:00 UTC" "2019-01-01 14:00:00 UTC" "2019-01-01 15:00:00 UTC"
# [17] "2019-01-01 16:00:00 UTC" "2019-01-01 17:00:00 UTC" "2019-01-01 18:00:00 UTC" "2019-01-01 19:00:00 UTC"
# [21] "2019-01-01 20:00:00 UTC" "2019-01-01 21:00:00 UTC" "2019-01-01 22:00:00 UTC" "2019-01-01 23:00:00 UTC"
```
