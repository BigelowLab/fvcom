FVCOM - GOM3
================

-   title: GOM3 Nesting by <Yf.Sun@umassd.edu>
-   institution: School for Marine Science and Technology
-   source: FVCOM\_3.0
-   history: model started at: 22/12/2011 16:41
-   references: <http://fvcom.smast.umassd.edu>,
    <http://codfish.smast.umassd.edu>

Set up what you need.

``` r
library(sf)
library(fvcom)
library(ncdf4)
library(dplyr)
library(kableExtra)
```

Open the connection.

``` r
uri <- "http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3"
x <- ncdf4::nc_open(uri)
```

What is in it?

``` r
vars <- fvcom::list_vars(x)
vars %>%
  kableExtra::kbl(caption = "The complete list of variables and dependencies") %>%
  kableExtra::kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
The complete list of variables and dependencies
</caption>
<thead>
<tr>
<th style="text-align:left;">
name
</th>
<th style="text-align:left;">
dims
</th>
<th style="text-align:left;">
dim1
</th>
<th style="text-align:left;">
dim2
</th>
<th style="text-align:left;">
dim3
</th>
<th style="text-align:left;">
units
</th>
<th style="text-align:left;">
longname
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nprocs
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
number of processors
</td>
</tr>
<tr>
<td style="text-align:left;">
partition
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
partition
</td>
</tr>
<tr>
<td style="text-align:left;">
x
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
meters
</td>
<td style="text-align:left;">
nodal x-coordinate
</td>
</tr>
<tr>
<td style="text-align:left;">
y
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
meters
</td>
<td style="text-align:left;">
nodal y-coordinate
</td>
</tr>
<tr>
<td style="text-align:left;">
lon
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
degrees\_east
</td>
<td style="text-align:left;">
nodal longitude
</td>
</tr>
<tr>
<td style="text-align:left;">
lat
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
degrees\_north
</td>
<td style="text-align:left;">
nodal latitude
</td>
</tr>
<tr>
<td style="text-align:left;">
xc
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
meters
</td>
<td style="text-align:left;">
zonal x-coordinate
</td>
</tr>
<tr>
<td style="text-align:left;">
yc
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
meters
</td>
<td style="text-align:left;">
zonal y-coordinate
</td>
</tr>
<tr>
<td style="text-align:left;">
lonc
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
degrees\_east
</td>
<td style="text-align:left;">
zonal longitude
</td>
</tr>
<tr>
<td style="text-align:left;">
latc
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
degrees\_north
</td>
<td style="text-align:left;">
zonal latitude
</td>
</tr>
<tr>
<td style="text-align:left;">
h
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
Bathymetry
</td>
</tr>
<tr>
<td style="text-align:left;">
nv
</td>
<td style="text-align:left;">
nele,three
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
three
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
nodes surrounding element
</td>
</tr>
<tr>
<td style="text-align:left;">
nbe
</td>
<td style="text-align:left;">
nele,three
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
three
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
elements surrounding each element
</td>
</tr>
<tr>
<td style="text-align:left;">
ntsn
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
\#nodes surrounding each node
</td>
</tr>
<tr>
<td style="text-align:left;">
nbsn
</td>
<td style="text-align:left;">
node,maxnode
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
maxnode
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
nodes surrounding each node
</td>
</tr>
<tr>
<td style="text-align:left;">
ntve
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
\#elems surrounding each node
</td>
</tr>
<tr>
<td style="text-align:left;">
nbve
</td>
<td style="text-align:left;">
node,maxelem
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
maxelem
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
elems surrounding each node
</td>
</tr>
<tr>
<td style="text-align:left;">
a1u
</td>
<td style="text-align:left;">
nele,four
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
four
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
a1u
</td>
</tr>
<tr>
<td style="text-align:left;">
a2u
</td>
<td style="text-align:left;">
nele,four
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
four
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
a2u
</td>
</tr>
<tr>
<td style="text-align:left;">
aw0
</td>
<td style="text-align:left;">
nele,three
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
three
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
aw0
</td>
</tr>
<tr>
<td style="text-align:left;">
awx
</td>
<td style="text-align:left;">
nele,three
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
three
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
awx
</td>
</tr>
<tr>
<td style="text-align:left;">
awy
</td>
<td style="text-align:left;">
nele,three
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
three
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
awy
</td>
</tr>
<tr>
<td style="text-align:left;">
art2
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Area of elements around a node
</td>
</tr>
<tr>
<td style="text-align:left;">
art1
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Area of Node-Base Control volume
</td>
</tr>
<tr>
<td style="text-align:left;">
iint
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
internal mode iteration number
</td>
</tr>
<tr>
<td style="text-align:left;">
Itime
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
days since 1858-11-17 00:00:00
</td>
<td style="text-align:left;">
Itime
</td>
</tr>
<tr>
<td style="text-align:left;">
Itime2
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
msec since 00:00:00
</td>
<td style="text-align:left;">
Itime2
</td>
</tr>
<tr>
<td style="text-align:left;">
Times
</td>
<td style="text-align:left;">
maxStrlen64,time
</td>
<td style="text-align:left;">
maxStrlen64
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Times
</td>
</tr>
<tr>
<td style="text-align:left;">
zeta
</td>
<td style="text-align:left;">
node,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
meters
</td>
<td style="text-align:left;">
Water Surface Elevation
</td>
</tr>
<tr>
<td style="text-align:left;">
file\_date
</td>
<td style="text-align:left;">
maxStrlen64,time
</td>
<td style="text-align:left;">
maxStrlen64
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
file\_date
</td>
</tr>
<tr>
<td style="text-align:left;">
u
</td>
<td style="text-align:left;">
nele,siglay,time
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
siglay
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
meters s-1
</td>
<td style="text-align:left;">
Eastward Water Velocity
</td>
</tr>
<tr>
<td style="text-align:left;">
v
</td>
<td style="text-align:left;">
nele,siglay,time
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
siglay
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
meters s-1
</td>
<td style="text-align:left;">
Northward Water Velocity
</td>
</tr>
<tr>
<td style="text-align:left;">
omega
</td>
<td style="text-align:left;">
node,siglev,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
siglev
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
s-1
</td>
<td style="text-align:left;">
Vertical Sigma Coordinate Velocity
</td>
</tr>
<tr>
<td style="text-align:left;">
ww
</td>
<td style="text-align:left;">
nele,siglay,time
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
siglay
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
meters s-1
</td>
<td style="text-align:left;">
Upward Water Velocity
</td>
</tr>
<tr>
<td style="text-align:left;">
ua
</td>
<td style="text-align:left;">
nele,time
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
meters s-1
</td>
<td style="text-align:left;">
Vertically Averaged x-velocity
</td>
</tr>
<tr>
<td style="text-align:left;">
va
</td>
<td style="text-align:left;">
nele,time
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
meters s-1
</td>
<td style="text-align:left;">
Vertically Averaged y-velocity
</td>
</tr>
<tr>
<td style="text-align:left;">
temp
</td>
<td style="text-align:left;">
node,siglay,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
siglay
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
degrees\_C
</td>
<td style="text-align:left;">
temperature
</td>
</tr>
<tr>
<td style="text-align:left;">
salinity
</td>
<td style="text-align:left;">
node,siglay,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
siglay
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
1e-3
</td>
<td style="text-align:left;">
salinity
</td>
</tr>
<tr>
<td style="text-align:left;">
km
</td>
<td style="text-align:left;">
node,siglev,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
siglev
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
m 2 s-1
</td>
<td style="text-align:left;">
Turbulent Eddy Viscosity For Momentum
</td>
</tr>
<tr>
<td style="text-align:left;">
kh
</td>
<td style="text-align:left;">
node,siglev,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
siglev
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
m 2 s-1
</td>
<td style="text-align:left;">
Turbulent Eddy Viscosity For Scalars
</td>
</tr>
<tr>
<td style="text-align:left;">
kq
</td>
<td style="text-align:left;">
node,siglev,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
siglev
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
m 2 s-1
</td>
<td style="text-align:left;">
Turbulent Eddy Viscosity For Q2/Q2L
</td>
</tr>
<tr>
<td style="text-align:left;">
q2
</td>
<td style="text-align:left;">
node,siglev,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
siglev
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
m2 s-2
</td>
<td style="text-align:left;">
Turbulent Kinetic Energy
</td>
</tr>
<tr>
<td style="text-align:left;">
q2l
</td>
<td style="text-align:left;">
node,siglev,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
siglev
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
m3 s-2
</td>
<td style="text-align:left;">
Turbulent Kinetic Energy X Turbulent Macroscale
</td>
</tr>
<tr>
<td style="text-align:left;">
l
</td>
<td style="text-align:left;">
node,siglev,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
siglev
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
m3 s-2
</td>
<td style="text-align:left;">
Turbulent Macroscale
</td>
</tr>
<tr>
<td style="text-align:left;">
short\_wave
</td>
<td style="text-align:left;">
node,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
W m-2
</td>
<td style="text-align:left;">
Short Wave Radiation
</td>
</tr>
<tr>
<td style="text-align:left;">
net\_heat\_flux
</td>
<td style="text-align:left;">
node,time
</td>
<td style="text-align:left;">
node
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
W m-2
</td>
<td style="text-align:left;">
Surface Net Heat Flux
</td>
</tr>
<tr>
<td style="text-align:left;">
uwind\_stress
</td>
<td style="text-align:left;">
nele,time
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Pa
</td>
<td style="text-align:left;">
Eastward Wind Stress
</td>
</tr>
<tr>
<td style="text-align:left;">
vwind\_stress
</td>
<td style="text-align:left;">
nele,time
</td>
<td style="text-align:left;">
nele
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Pa
</td>
<td style="text-align:left;">
Northward Wind Stress
</td>
</tr>
<tr>
<td style="text-align:left;">
fvcom\_mesh
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
fvcom\_mesh
</td>
</tr>
</tbody>
</table>

Geometry locations can be retrieved at `nodes` or `elements`.

``` r
epts = fvcom::fvcom_elems(x)
epts
```

    ## # A tibble: 90,415 × 3
    ##     elem   lon   lat
    ##    <int> <dbl> <dbl>
    ##  1     1 -59.9  46.1
    ##  2     2 -59.8  46.1
    ##  3     3 -59.8  46.1
    ##  4     4 -59.8  46.1
    ##  5     5 -59.8  46.0
    ##  6     6 -59.7  46.0
    ##  7     7 -59.7  46.0
    ##  8     8 -59.7  45.9
    ##  9     9 -59.6  45.9
    ## 10    10 -59.6  45.9
    ## # … with 90,405 more rows

You can also collect the neighboring indices for elements and/or nodes.
These are returned as matrices that have the indices of each neighboring
element or node. Below we show what is returned when you include
neighboring elements. We use `str()` below because Rmarkdown doesn’t
know how to print a tibble with a matrix column.

``` r
epts = fvcom::fvcom_elems(x, include = "nbe")
str(epts)
```

    ## tibble [90,415 × 4] (S3: tbl_df/tbl/data.frame)
    ##  $ elem: int [1:90415] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ lon : num [1:90415(1d)] -59.9 -59.8 -59.8 -59.8 -59.8 ...
    ##  $ lat : num [1:90415(1d)] 46.1 46.1 46.1 46.1 46 ...
    ##  $ nbe : int [1:90415, 1:3] 393 3 0 3 6 7 6 388 0 9 ...

Don’t forget to tidy up when you are done!

``` r
ncdf4::nc_close(x)
```
