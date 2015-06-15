library(sp, lib.loc = "C:/inst/R/x64/library")
library(rgdal, lib.loc = "C:/inst/R/x64/library")

sessionInfo()
R version 2.11.0 beta (2010-04-07 r51635)
x86_64-pc-mingw32

locale:
[1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252
[3] LC_MONETARY=English_Australia.1252 LC_NUMERIC=C
[5] LC_TIME=English_Australia.1252

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
[1] rgdal_0.6-26 sp_0.9-61

loaded via a namespace (and not attached):
[1] grid_2.11.0    lattice_0.18-4


## Windows 7, Intel i7 CPU, 12Gb RAM

## ==== RASTER

## 21600 x 10800 1-band raster
topo <- readGDAL("E:/DATA/Etopo/Etopo2Ice/Etopo1.tif")

summary(topo)

print(object.size(topo), units = "Mb")
#889.9 Mb


## write with options
tf <-  tempfile()
writeGDAL(topo, tf, options = c("COMPRESS=DEFLATE", "TILED=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=1024"))

#system(paste("gdalinfo", tf))
#### GDAL 1.7.0b2, FWTools 2.4.7, released 2010/01/19
#Driver: GTiff/GeoTIFF
#Files: G:\SYSTEM~1\RtmpCApbyb\file3d6c4ae1
#Size is 21600, 10800
#Coordinate System is:
#GEOGCS["WGS 84",
#    DATUM["WGS_1984",
#        SPHEROID["WGS 84",6378137,298.257223563,
#            AUTHORITY["EPSG","7030"]],
#        AUTHORITY["EPSG","6326"]],
#    PRIMEM["Greenwich",0],
#    UNIT["degree",0.0174532925199433],
#    AUTHORITY["EPSG","4326"]]
#Origin = (-180.000000000000000,90.000000000000000)
#Pixel Size = (0.016666666666667,-0.016666666666667)
#Metadata:
#  AREA_OR_POINT=Area
#Image Structure Metadata:
#  COMPRESSION=DEFLATE
#  INTERLEAVE=BAND
#Corner Coordinates:
#Upper Left  (-180.0000000,  90.0000000) (180d 0'0.00"W, 90d 0'0.00"N)
#Lower Left  (-180.0000000, -90.0000000) (180d 0'0.00"W, 90d 0'0.00"S)
#Upper Right ( 180.0000000,  90.0000000) (180d 0'0.00"E, 90d 0'0.00"N)
#Lower Right ( 180.0000000, -90.0000000) (180d 0'0.00"E, 90d 0'0.00"S)
#Center      (   0.0000000,   0.0000000) (  0d 0'0.01"E,  0d 0'0.01"N)
#Band 1 Block=512x1024 Type=Float32, ColorInterp=Gray


## check the write with a connection
x <- GDAL.open(tf)
displayDataset(x, reduction = 100)
displayDataset(x)

## common binary format
tf <- tempfile()
writeGDAL(topo, tf, drivername = "EHdr")

## ==== VECTOR

## SDTS
countries <- "E:/DATA/Countries/Councatd.ddf"
layer <- "PC01"

ogrInfo(countries, layer)
#Source: "E:/DATA/Countries/Councatd.ddf", layer: "PC01"
#Driver: SDTS number of rows 244
#Feature type: wkbPolygon with 3 dimensions
#+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs
#Number of fields: 7
#                              name type length typeName
#1                             RCID    0      0  Integer
#2                               ID    0      0  Integer
#3                          Country    4      0   String
#4                          Capital    4      0   String
#5                             FIPS    4      0   String
#6     Member of the United Nations    0      0  Integer
#7 Diplomatic Relations with the US    0      0  Integer

d <- readOGR(countries, layer)
summary(d)


## SHP
## very encouraging speed here (due to Roger's recent updates AFAIK)
countries <- "E:/DATA/Countries"
layer <- "Countries-hires"

system.time(d <- readOGR(countries, layer))
#OGR data source with driver: ESRI Shapefile
#Source: "E:/DATA/Countries", layer: "Countries-hires"
#with 244 features and 6 fields
#Feature type: wkbPolygon with 2 dimensions
#   user  system elapsed
#   5.54    0.76    6.30
print(object.size(d), units = "Mb")
#115.4 Mb

## get all vertices from polygons
xy <- do.call("rbind",
	lapply(d@polygons, function(x1) do.call("rbind",
		lapply(x1@Polygons, function(x2) x2@coords[-nrow(x2@coords), ]))))
dim(xy)
#[1] 4937426       2

## reproject and plot
plot(spTransform(d, CRS("+proj=laea +lon_0=147 +lat_0=-42")))


epsg <- make_EPSG()

length(grep("laea", epsg$prj4))
#[1] 10

example(readGDAL)
example(writeGDAL)
example(GDAL.open)
example(project)
example(readOGR)
example(writeOGR)




## ===== VERY LARGE POLYGON FILE

## Very large shapefile, takes time and eventually failed - but not surprising
## I will try some more reasonable sizes
## 883 Mb of file (many vectorized pixels, really inefficient format)
system.time(d <- readOGR("G:\\DATA\\LARGESHP)\\V2", "Bathy"))


