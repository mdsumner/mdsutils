library(foreach)
x <- matrix(runif(5000), 1000)
y <- gl(20, 50)
library(randomForest)

rf <- foreach(ntree = rep(250, 4), .combine = combine) %do% randomForest(x, y, ntree = ntree)


rf <- foreach(ntree = rep(250, 4), .combine = combine, .packages = "randomForest") %dopar% randomForest(x, y, ntree = ntree)




library(foreach)
applyKernel <- function(newX, FUN, d2, d.call, dn.call = NULL, ...) {
 foreach(x = iblkcol(newX, 3), .combine = "c", .packages = "foreach") %dopar%
 {

 foreach(i = 1:ncol(x)) %do% FUN(array(x[, i], d.call, dn.call), ...)
 }
 }

applyKernel(matrix(1:16, 4), mean, 4, 4)





netcdf.bbox <- function(file, variable, bbox, outfile = NULL) {
    require(RNetCDF)

    nc <- open.nc(file)
    varinfo <- var.inq.nc(nc, variable)
    vardiminfo <- lapply(varinfo$dimids, function(x) dim.inq.nc(nc, x))
    vardim.df <- do.call("rbind",
                         lapply(vardiminfo, as.data.frame,
                                stringsAsFactors = FALSE))

    vardim.vectors <- sapply(vardim.df$name, function(x) var.get.nc(nc, x))
    nms <- names(vardim.vectors)

    vardim.indexes <- sapply(vardim.vectors, function(x) 1:length(x))

    for (i in 1:nrow(bbox)) {
        vardim.indexes[[nms[i]]] <- which(vardim.vectors[[i]] > bbox[i, 1]
                                          & vardim.vectors[[i]] < bbox[i, 2])
    }

    starts <- unlist(lapply(vardim.indexes, min))
    counts <- unlist(lapply(vardim.indexes, length))
    vardata <- var.get.nc(nc, variable, start = starts, count = counts)

    close.nc(nc)
    for (i in 1:length(vardim.indexes)) {
        vardim.vectors[[i]] <- vardim.vectors[[i]][vardim.indexes[[i]]]
    }

    dimvartype <- "NC_DOUBLE"
    if (!is.null(outfile)) {

        nc <- create.nc(outfile)
        for (i in 1:length(vardim.vectors)) {
            dim.def.nc(nc, names(vardim.vectors)[i], length(vardim.vectors[[i]]))
            var.def.nc(nc, names(vardim.vectors)[i], if (i < 2) dimvartype else "NC_INT", i - 1)
            var.put.nc(nc, names(vardim.vectors)[i], vardim.vectors[[i]])
        }
        var.def.nc(nc, variable, "NC_DOUBLE", 0:(length(dim(vardata))-1))
        var.put.nc(nc, variable, vardata)
        close.nc(nc)
        return(outfile)
    }
    vardim.vectors[[variable]] <- vardata
    vardim.vectors
}

file <- "E:/DATA/Reynolds/sst.wkmean.1990-present.nc"
variable <- "sst"

## set bounding box, of each dimension in order (we don't care about time so we leave it out)
bbox <- rbind(xlim = c(lon.min, lon.max), ylim = c(lat.min, lat.max), tlim = (unclass(range(d$gmt) + c(-1,1) * 7 * 24 * 3600) - unclass(as.POSIXct("1800-1-1 00:00:00", tz = "GMT"))) /  ( 24 * 3600)
)

## write out to a new file
sst <- netcdf.bbox(file, variable, bbox)

sst$sst <- sst$sst * 0.01
sst$lat <- rev(sst$lat)
for (i in 1:dim(sst$sst)[3]) sst$sst[,,i] <- sst$sst[,dim(sst$sst)[2]:1,i] 
sst$time <- as.POSIXct("1800-1-1 00:00:00", tz = "GMT") + sst$time * 24 * 3600


tempok <- !is.na(d$temp)
afun <- approxfun(d$gmt[tempok], d$temp[tempok], rule = 2)
d$atemp <- afun(d$gmt)
d1$atemp <- afun(d1$gmt)

temps <- tapply(d1$atemp, d1$segment, mean)

tm <- tapply(d1$gmt, d1$segment, mean)

for (i in 1:(m-2)) {
	this.sst.ind <- findInterval(tm[i], sst$time)

	this.sst <- interp(sst$sst[,,this.sst.ind], sst$sst[,,this.sst.ind + 1], calcProportion(sst$time[this.sst.ind], sst$time[this.sst.ind + 1], tm[i]))

	image(sst$lon, sst$lat, abs(this.sst - temps[i]) < 1)
	lines(ch$last.x[1:i, ])
	#scan()

}



## now make the mask - the masks stuffed into integer bits
nsegs <- m

nblocks <- (nsegs%/%31) + 1
            nlon <- length(sst$lon)
            nlat <- length(sst$lat)
            Mask <- list(x = sst$lon, y = sst$lat, z = array(integer(nblocks *
                nlat * nlon), c(nlon, nlat, nblocks)))

	
    for (k in 1:nsegs) {
        blk <- (k%/%31) + 1
        bit <- (k - 1)%%31
    
	if (k == 1 | k == m) {
	        bits(Mask$z[, , blk], bit) <- matrix(TRUE, nlon, nlat)
	} else {
		this.sst.ind <- findInterval(tm[k - 1], sst$time)

	this.sst <- interp(sst$sst[,,this.sst.ind], sst$sst[,,this.sst.ind + 1], calcProportion(sst$time[this.sst.ind], sst$time[this.sst.ind + 1], tm[k - 1]))

		bits(Mask$z[, , blk], bit) <- abs(this.sst - temps[k - 1]) < 1
	}
	
}

for (i in 1:m) image(get.mask(Mask, i))

sst.ok <- mkLookup(Mask, TRUE)
land.ok <- mkLookup(topo, FALSE)

## should work . . ..
lookup <- function(xy) land.ok(xy) & sst.ok(xy)



interp <- function(x1, x2, proportion) {
    x1 * (1 - proportion) + x2 * proportion
}


calcProportion <-
function(xmin, xmax, x) {
    (x - unclass(xmin) ) / (unclass(xmax) - unclass(xmin))
}
