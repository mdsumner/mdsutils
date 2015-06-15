
## the data is in one big vector, so we need to do our own X/Y indexing


#### 1. I can't test this, so let me know everything that goes right/wrong

library(RNetCDF)

## open the file
ncfile <- open.nc("gebco_08.nc")

## let's ignore the stuff in the file, the description is enough

## X/Y coordinates
xs <- seq(-(179 + 59/60 + 45/3600), 179 + 59/60 + 45/3600, length = 43200)
ys <- seq(-(89 + 59/60 + 45/3600), 89 + 59/60 + 45/3600, length = 21600)


## X/Y indexes (Y is reversed so we catch that later)
xind <- which(xs >= 77 & xs <= 142)
yind <- which(ys >= -69 & ys <= -65)


## we can ignore this many "rows"
skipRows <- (length(ys) - max(yind) - 1) * length(xs)


result <- matrix(0.0, length(xind), length(yind))

## now let's read each row, 43200 at a time, but indexing just the xs we want

for (i in 1:ncol(result)) {
	thisrow <- var.get.nc(ncfile, "z", start = skipRows + (i - 1) * length(xs), count = length(xs))
	result[,ncol(result) - i + 1 ] <- thisrow[xind]
}

## clean up
close.nc(ncfile)


#### 2. replace that with an image() structure
result <- list(x = xs[xind], y = ys[yind], z = result)

## does that look right? 
library(tripEstimation)
library(mapdata)
image(mkSmall(result), 5)
map('worldHires', add = TRUE)


#### 3. if all is well, convert to SpatialGridDataFrame
library(sp)
result <- image2Grid(result, p4 = "+proj=longlat +ellps=WGS84")




readGEBCObathy <- function(filename = "gebco_08.nc", xlim = NULL, ylim = NULL, sgdf = TRUE) {

	if (!require(RNetCDF) ) stop("RNetCDF package is not installed")
	if (!file.exists(filename)) stop(filename, "not found")

	## open the file
	ncfile <- try(open.nc(filename))
	## clean up
	on.exit(close.nc(ncfile))
	if ( inherits(ncfile, "try-error")) stop("could not open as NetCDF", filename, "\n\n", ncfile)


	## let's ignore the stuff in the file, the description is enough

	## X/Y coordinates
	xs <- seq(-(179 + 59/60 + 45/3600), 179 + 59/60 + 45/3600, length = 43200)
	ys <- seq(-(89 + 59/60 + 45/3600), 89 + 59/60 + 45/3600, length = 21600)

	if (is.null(xlim)) xlim <- range(xs)
	if (is.null(ylim)) ylim <- range(ys)

	## X/Y indexes (Y is reversed so we catch that later)
	xind <- which(xs >= xlim[1] & xs <= xlim[2])
	yind <- which(ys >= ylim[1] & ys <= ylim[2])

	if (length(xind) < 1) stop("problem with X range")
	result <- try(matrix(0.0, length(xind), length(yind)))
	if (inherits(result, "try-error")) stop("could not create matrix of size", c(length(xind), length(yind)))

	
	## we can ignore this many values reading rows from north to south
	skipElements <- (length(ys) - max(yind) ) * length(xs)



	## now let's read each row, 43200 at a time, but indexing just the xs we want
        ## and reversing the Y direction for R's image() orientation

	for (i in 1:ncol(result)) {
	
	
		
		thisrow <- var.get.nc(ncfile, "z", start = skipElements + (i - 1) * length(xs), count = length(xs))
		result[,ncol(result) - i + 1 ] <- thisrow[xind]
	}

	


	#### 2. replace that with an image() structure
	result <- list(x = xs[xind], y = ys[yind], z = result)

	##SGDF
	if (sgdf) {
		if (!require(sp) ) stop("sp package is not installed and is required by sgdf = TRUE")
		result <- image2Grid(result, p4 = "+proj=longlat +ellps=WGS84")
	}
	return(result)

}

## does that look right? 
library(tripEstimation)
library(mapdata)
a <- readGEBCObathy(xlim = c(77, 142), ylim = c(-70, -60))
image(mkSmall(as.image.SpatialGridDataFrame(a), 5))

map('worldHires', add = TRUE)

## it's well worth using these options (TILED, and LZW compression) it will make reprojection with gdalwarp more efficient
library(rgdal)
writeGDAL(a, "gebco.tif", options = c("TILED=YES", "COMPRESS=LZW"))





a <- readGEBCObathy(xlim = c(-180, -170), ylim = c(80, 90))


a <- readGEBCObathy(xlim = c(170, 180), ylim = c(-90, -80))
image(mkSmall(as.image.SpatialGridDataFrame(a), 5))

map('worldHires', add = TRUE)



a <- readGEBCObathy(xlim = c(-180, -170), ylim = c(80, 90))
image(mkSmall(as.image.SpatialGridDataFrame(a), 5))

map('worldHires', add = TRUE)





