### temperature data

Temperature mask

The simplest mask function consists of a single matrix of values that
exclude land regions. This can be extended to provide "seasonal"
variations



interp <- function(x1, x2, proportion) {
    x1 * (1 - proportion) + x2 * proportion
}
calcProportion <-
function(xmin, xmax, x) {
    (x - unclass(xmin) ) / (unclass(xmax) - unclass(xmin))
}

library(RODBC)
con <- odbcConnectAccess("G:/GEM/temperature.model/Macca2005.mdb")

qu <- "SELECT LON, LAT, END_DATE, TEMP_DBAR, TEMP_VALS, ref FROM ctd WHERE ref =  'ct6_10015_05 '"
d <- sqlQuery(con, qu)
close(con)

library(trip)
coordinates(d) <- ~LON+LAT
## TODO: make "forceCompliance" an argument to trip()
d <- forceCompliance(d,  c("END_DATE", "ref"))

names(d) <- c("gmt", "depth", "temp", "ref")

temps <- lapply(strsplit(as.character(d$temp), ","), as.numeric)
depths <- lapply(strsplit(as.character(d$depth), ","), as.numeric)
bb <- bbox(d) + rbind(c(-4, 4), c(-2, 2))

plot(d)
lines(coordinates(d))

## OISST
library(RNetCDF)
nc <- open.nc("E:/DATA/Reynolds/sst.wkmean.1990-present.nc")
usegem <- FALSE
time <- var.get.nc(nc, "time") * 24 * 3600 + as.POSIXct("1800-1-1 00:00:00")
timelim <- findInterval(range(d$gmt), time) + c(-1, 1)
x <- var.get.nc(nc, "lon")
xlim <- findInterval(bb[1,], x)  + c(-1, 1)
y <- rev(var.get.nc(nc, "lat"))
ylim <- findInterval(bb[2,], y) + c(-1, 1)

sst <- list(x = x[min(xlim):max(xlim)],
            y = y[min(ylim):max(ylim)],
            t = time[timelim[1]:timelim[2]],
            sst = var.get.nc(nc, "sst", c(xlim[1], length(y) - ylim[1] - diff(ylim) + 1, timelim[1]),
            c(diff(xlim)+1, diff(ylim)+1, diff(timelim)+1))[,(diff(ylim)+1):1,] * 0.01
            )
close.nc(nc)

nc <- open.nc("E:/DATA/Reynolds/lsmask.nc")
landsea <- var.get.nc(nc, "mask", c(xlim[1], length(y) - ylim[1] - diff(ylim) + 1, 1), c(diff(xlim)+1, diff(ylim)+1, 1))[,(diff(ylim)+1):1]
close.nc(nc)




## GEM 3D
source("G:/GEM/readGEM.R")
usegem <- TRUE
gem <- readGEM(xlim = bb[1,], ylim = bb[2,], zlim = range(unlist(depths)) + c(-100, 100), tlim = range(d$gmt) + c(-7*24*3600, 7*24 * 3600))
sst <- list(x = gem$x, y = gem$y, t = gem$t, sst = gem$temp_depth[,,1,,drop = TRUE])

image(sst$x, sst$y, sst$sst[,,1])
lines(coordinates(d))


mask <- array(FALSE, c(length(sst$x), length(sst$y), nrow(d)))

##temprange <- matrix(unlist(lapply(temps, range)), ncol = 2, byrow = TRUE) + cbind(rep(-0.6, nrow(d)), rep(0.6, nrow(d)))
##temprange1 <- temprange
temprange <- matrix(unlist(lapply(temps, function(x) sort(x[1:2]))), ncol = 2, byrow = TRUE) + cbind(rep(-1, nrow(d)), rep(1, nrow(d)))


## exact interpolation
for (i in 1:nrow(d)) {
    tindex <- findInterval(d$gmt[i], sst$t)

    prop <- calcProportion(unclass(sst$t[tindex]), unclass(sst$t[tindex + 1]), unclass(d$gmt[i]))

    this.sst <- interp(sst$sst[,,tindex], sst$sst[,,tindex + 1], prop)
    if (usegem) landsea <- !is.na(this.sst)

    mask[,,i] <- this.sst > temprange[i,1] & this.sst < temprange[i,2] & landsea
    image(mask[,,i])

}


bmask <- mask
## union of bounding cells
for (i in 1:nrow(d)) {
    tindex <- findInterval(d$gmt[i], sst$t)

    prop <- calcProportion(unclass(sst$t[tindex]), unclass(sst$t[tindex + 1]), unclass(d$gmt[i]))

    this.sst1 <- sst$sst[,,tindex]
    this.sst2 <- sst$sst[,,tindex + 1]
    if (usegem) landsea <- !is.na(this.sst1) | !is.na(this.sst2)

    bmask[,,i] <- (this.sst1 > temprange[i,1] & this.sst1 < temprange[i,2]) | (this.sst2 > temprange[i,1] & this.sst2 < temprange[i,2]) & landsea
    image(bmask[,,i])

}
for (i in 1:nrow(d)) {image(mask[,,i]);contour(bmask[,,i], lev = 1, add = T);scan()}



## GEM 4D
source("G:/GEM/readGEM.R")
usegem <- TRUE
gem <- readGEM(xlim = bb[1,], ylim = bb[2,], zlim = range(unlist(depths)) + c(-100, 100), tlim = range(d$gmt) + c(-7*24*3600, 7*24 * 3600))
sst <- list(x = gem$x, y = gem$y, z = gem$z, t = gem$t, sst = gem$temp_depth)

image(sst$x, sst$y, sst$sst[,,1])
lines(coordinates(d))


mask <- array(FALSE, c(length(sst$x), length(sst$y), nrow(d)))
msk <- mask[,,1]


##temprange <- matrix(unlist(lapply(temps, range)), ncol = 2, byrow = TRUE) + cbind(rep(-0.6, nrow(d)), rep(0.6, nrow(d)))
##temprange1 <- temprange
##temprange <- matrix(unlist(lapply(temps, function(x) sort(x[1:2]))), ncol = 2, byrow = TRUE) + cbind(rep(-1, nrow(d)), rep(1, nrow(d)))


## exact interpolation
for (i in 1:nrow(d)) {
    tindex <- findInterval(d$gmt[i], sst$t)

    timeprop <- calcProportion(unclass(sst$t[tindex]), unclass(sst$t[tindex + 1]), unclass(d$gmt[i]))
    z.ind <- pmax(1, findInterval(depths[[1]], sst$z))
    msk[] <- TRUE
    for (j in unique(z.ind)) {
        trange <- range(temps[[i]][z.ind == j]) + c(-2, 2)
        this.temp1 <- interp(sst$sst[,,j,tindex], sst$sst[,,j,tindex + 1], timeprop)
        this.temp1[is.na(this.temp1)] <- trange[1] - 100
        #this.temp2 <- interp(sst$sst[,,j+1,tindex], sst$sst[,,j+1,tindex + 1], timeprop)

        msk <- msk & this.temp1 > trange[1] & this.temp1 < trange[2]
        #image(msk)
        #scan()
    }
    mask[,,i] <- msk

    image(sst$x, sst$y, mask[,,i])
    lines(coordinates(d)[1:i, ])
    scan()

}


bmask <- mask
## union of bounding cells
for (i in 1:nrow(d)) {
    tindex <- findInterval(d$gmt[i], sst$t)

    prop <- calcProportion(unclass(sst$t[tindex]), unclass(sst$t[tindex + 1]), unclass(d$gmt[i]))

    this.sst1 <- sst$sst[,,tindex]
    this.sst2 <- sst$sst[,,tindex + 1]
    if (usegem) landsea <- !is.na(this.sst1) | !is.na(this.sst2)

    bmask[,,i] <- (this.sst1 > temprange[i,1] & this.sst1 < temprange[i,2]) | (this.sst2 > temprange[i,1] & this.sst2 < temprange[i,2]) & landsea
    image(bmask[,,i])

}
for (i in 1:nrow(d)) {image(mask[,,i]);contour(bmask[,,i], lev = 1, add = T);scan()}





xy <- as.matrix(expand.grid(x = sst$x, y = sst$y))

start.point <- coordinates(d)[1,, drop = FALSE]
last.xy <- start.point
mask2 <- mask
max.dist <- 200

for (i in 1:nrow(d)) {
    ok <- as.vector(mask[,,i])
    ok.ind <- which(ok)
    this.xy <- xy[ok, , drop = FALSE]
    dst <- spDists(this.xy, last.xy, longlat = TRUE)
    ok.dst <- apply(dst, 1, function(x) any(x[x > 0] < max.dist))
    last.xy <- this.xy[ok.dst, ,drop = FALSE]
    ok[-ok.ind[ok.dst]] <- FALSE
    mask2[,,i] <- ok
    if (nrow(last.xy) == nrow(this.xy)) break;
    image(sst$x,sst$y, mask[,,i])
    points(last.xy)
   # scan()
}
last.xy <- start.point

for (i in nrow(d):1) {
    ok <- as.vector(mask[,,i])
    ok.ind <- which(ok)
    this.xy <- xy[ok, , drop = FALSE]
    dst <- spDists(this.xy, last.xy, longlat = TRUE)
    ok.dst <- apply(dst, 1, function(x) any(x[x > 0] < max.dist))
    last.xy <- this.xy[ok.dst, ,drop = FALSE]
    ok[-ok.ind[ok.dst]] <- FALSE
    mask2[,,i] <- ok
    if (nrow(last.xy) == nrow(this.xy)) break;
    image(sst$x,sst$y, mask[,,i])
    points(last.xy)
   # scan()
}

for (i in 2:nrow(d)) {image(sst$x, sst$y, mask2[,,i]);lines(coordinates(d)[1:i, ])}

i <- 1

    tindex <- findInterval(d$gmt[i], sst$t)

    prop <- calcProportion(unclass(sst$t[tindex]), unclass(sst$t[tindex + 1]), unclass(d$gmt[i]))

    this.sst <- interp(sst$sst[,,tindex], sst$sst[,,tindex + 1], prop)

scl <- function(x) (x - min(x))/diff(range(x))


col <- col2rgb(rainbow(256))

 a <- array(scl(t(this.sst)[ncol(this.sst):1,]), c(dim(t(this.sst)), 3))

a[,,1] <- col[1,as.vector(scl(t(this.sst)[ncol(this.sst):1,]) * 255) + 1]
a[,,2] <- col[2,as.vector(scl(t(this.sst)[ncol(this.sst):1,]) * 255) + 1]
a[,,3] <- col[3,as.vector(scl(t(this.sst)[ncol(this.sst):1,]) * 255) + 1]

a <- scl(a)

raster(a, 0, 0, 1, 1)
contour(this.sst, add = T, drawlabels = FALSE)






## BOGUS Polygon stuff - too hard
for (i in 1:nrow(temprange)) {

    tindex <- findInterval(d$gmt[i], sst$t)
    prop <- calcProportion(unclass(sst$t[tindex]), unclass(sst$t[tindex + 1]), unclass(d$gmt[i]))
    this.sst <- interp(sst$sst[,,tindex], sst$sst[,,tindex + 1], prop)
    lns <- contourLines(sst$x, sst$y, this.sst, level = temprange[i,])
if (length(lns) < 2) next
    crds <- matrix(0, ncol = 2, nrow = sum(unlist(lapply(lns[1:2], function(x) length(x[["x"]])))))
    ord <- lns[[1]]$x[1] < lns[[1]]$x[length(lns[[1]]$x)]
    if (!ord) crds[nrow(crds):(1 + length(lns[[1]][["x"]])), ] <- cbind(lns[[2]]$x, lns[[2]]$y) else
    crds[(1 + length(lns[[1]][["x"]])):nrow(crds), ] <- cbind(lns[[2]]$x, lns[[2]]$y)
    crds[1:length(lns[[1]][["x"]]), ] <- cbind(lns[[1]]$x, lns[[1]]$y)

    image(sst$x, sst$y, this.sst)
    plot(SpatialPolygons(list(Polygons(list(Polygon(rbind(crds, crds[1,]), FALSE)), "1"))), col = "grey", add = T)


}








load("E:/DATA/bunbury.Rdata")
raster.SGDF <- function (x, attr = 1, xcol = 1, ycol = 2, col = heat.colors(12),
    red = NULL, green = NULL, blue = NULL, axes = FALSE, xlim = NULL,
    ylim = NULL, add = FALSE, ..., asp = NA, setParUsrBB = FALSE, interpolate = FALSE, angle = 0)
{
    ## function to scale values to [0, 1]
    scl <- function(x) (x - min(x, na.rm  = TRUE))/diff(range(x, na.rm = TRUE))
    ## bounding box of the image
    bb <- bbox(x)
    ## set up the background plot if it's not already
    if (!add)
        plot(as(x, "Spatial"), xlim = xlim, ylim = ylim, axes = axes,
            asp = asp, ..., setParUsrBB = setParUsrBB)
    ## 1-band case
    if (is.null(red)) {
        x <- x[attr]
        NAs <- is.na(x[[1]])
        nvalues <- length(unique(x[[1]][!NAs]))
        m <-  scl(t(matrix(x[[1]], x@grid@cells.dim[1], x@grid@cells.dim[2])))
        m <- matrix(col[as.vector(m) * (length(col)-1) + 1], nrow(m), ncol(m))
        ## if missing, set to white
        m[is.na(m)] <- rgb(255, 255, 255, 255, max = 255)
    } else {
        ## 3-band RGB case
        if (is.null(green) || is.null(blue))
            stop("all colour bands must be given")
        ## band data and missing values
        xd <- x@data[, c(red, green, blue)]
        #if (!is.null(alpha)) xd$alpha <- x@data[, alpha] else xd$alpha <- 255
        ## what about alpha NAs?
        NAs <- is.na(xd[, 1]) | is.na(xd[, 2]) | is.na(xd[, 3]) #| is.na(xd[,4])
        if (any(NAs))
            xd <- xd[!NAs, ]
        ## create RGBs (using alpha=1 by default)
        xd$alpha <- 255
        RGBs <- rgb(xd[[1]], xd[[2]], xd[[3]], maxColorValue = 255)
        if (any(NAs)) {
            z <- rep(NA, length(NAs))
            z[!NAs] <- RGBs
            RGBs <- z
        }
        cv <- coordinatevalues(getGridTopology(x))
        m <- t(matrix(RGBs, x@grid@cells.dim[1], x@grid@cells.dim[2],
        byrow = FALSE))
    }
browser()
    raster(m, bb[1,1], bb[2,1], bb[1,2], bb[2,2], interpolate = interpolate, angle = angle)
}
library(sp, lib.loc = "C:/inst/R/x64/library")
raster.SGDF(im, red = "band1", green = "band2", blue = "band3")




  library(sp)  ## R 2.11.0
  data(Rlogo)
  d = dim(Rlogo)
  cellsize = abs(c(gt[2],gt[6]))
  cells.dim = c(d[1], d[2]) # c(d[2],d[1])
  cellcentre.offset = c(x = gt[1] + 0.5 * cellsize[1], y = gt[4] - (d[2] - 0.5) * abs(cellsize[2]))
  grid = GridTopology(cellcentre.offset, cellsize, cells.dim)
  df = as.vector(Rlogo[,,1])
  for (band in 2:d[3]) df = cbind(df, as.vector(Rlogo[,,band]))
  df = as.data.frame(df)
  names(df) = paste("band", 1:d[3], sep="")
  Rlogo <- SpatialGridDataFrame(grid = grid, data = df)

op <- par(mfrow = c(3, 1))
raster.SGDF(Rlogo, red = "band1", green = "band1", blue = "band3", interpolate = FALSE)

raster.SGDF(Rlogo, col = grey(seq(0, 1, length = 10)), angle = 28)
raster.SGDF(Rlogo, col = grey(seq(0, 1, length = 10)), interpolate = TRUE)
par(op)

 sessionInfo()
R version 2.11.0 Under development (unstable) (2010-03-07 r51225)
x86_64-pc-mingw64

locale:
[1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252
[3] LC_MONETARY=English_Australia.1252 LC_NUMERIC=C
[5] LC_TIME=English_Australia.1252

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
[1] sp_0.9-60

loaded via a namespace (and not attached):
[1] grid_2.11.0    lattice_0.18-3 tools_2.11.0



as.raster.SpatialGridDataFrame <- function(x, col = heat.colors(12)) {


    library(sp, lib.loc = "C:/inst/R/x64/library")
    data(meuse.grid)
    m = SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid)

    x <- as(m["soil"], "SpatialGridDataFrame")

    scl <- function(x) (x - min(x, na.rm  = TRUE))/diff(range(x, na.rm = TRUE))


    ## is this right?
    bb <- bbox(x)
    nvalues <- length(unique(x[[1]][!is.na(x[[1]])]))
    #if (is.null(colors)) {
    #    colors <- heat.colors(nvalues)
    #    #colors <- gsub("FF$", "", colors)
    #}

   m <-  scl(t(matrix(x[[1]], x@grid@cells.dim[1], x@grid@cells.dim[2])))

   a <- matrix(col[as.vector(m) * (length(col)-1) + 1], nrow(m), ncol(m))
   a[is.na(m)] <- rgb(1, 1, 1, 1)

    a <- substr(a, 1, 7)

    #png(width = 1000, height = 1000)
    plot(1, xlim = bb[1,], ylim = bb[2,])

    raster(a, bb[1,1], bb[2,1], bb[1,2], bb[2,2], interpolate = FALSE)
    #dev.off()


}



a <- matrix(heat.colors(50), 5, 10)
plot(1)
raster(a, 1e4, 1e4, 2e4, 2e4)






start.point <- coordinates(d)[1,, drop = FALSE]

xy <- as.matrix(expand.grid(x = sst$x, y = sst$y))
last.xy <- start.point
max.dist <- 150
mask2 <- mask

for (i in 1:nrow(d)) {
    #if (i == 16) break
    ok <- as.vector(mask[,,i])
    ok.ind <- which(ok)

    this.xy <- xy[ok, , drop = FALSE]
    image(sst$x, sst$y, mask[,,i])
    points(this.xy)
    count <- rep(0, length(ok.ind))
    for (j in 1:nrow(last.xy)) {
        distances <- spDistsN1(this.xy, last.xy[j,,drop = FALSE], longlat = TRUE)
        count <- count + (distances < (max.dist)) * 1

    }
#    ok[ok.ind[!count == nrow(last.xy)]] <- FALSE
    ok[ok.ind[!count > 0]] <- FALSE
    #dst <- spDists(this.xy, last.xy, longlat = TRUE)
    #ok.dst <- apply(dst, 1, function(x) min(x[x > 0]) < max.dist)
    last.xy <- xy[ok, ,drop = FALSE]
    points(last.xy, pch = 16)
    mean.point <- apply(last.xy, 2, mean)
    star.points <-  gcDestination(mean.point[1], mean.point[2], seq(0, 360, by = 6), dist = max.dist)
    for (n in 1:nrow(star.points)) {
        lines(rbind(mean.point, star.points[n,]))
    }
    #scan()
    mask2[,,i] <- ok
    if (nrow(last.xy) == nrow(this.xy)) break;
}



xy <- as.matrix(expand.grid(x = sst$x, y = sst$y))
last.xy <- start.point
for (i in nrow(d):1) {
    #if (i == 16) break
    ok <- as.vector(mask[,,i])
    ok.ind <- which(ok)

    this.xy <- xy[ok, , drop = FALSE]
    image(sst$x, sst$y, mask[,,i])
    points(this.xy)
    count <- rep(0, length(ok.ind))
    for (j in 1:nrow(last.xy)) {
        distances <- spDistsN1(this.xy, last.xy[j,,drop = FALSE], longlat = TRUE)
        count <- count + (distances < (max.dist)) * 1

    }
#    ok[ok.ind[!count == nrow(last.xy)]] <- FALSE
    ok[ok.ind[!count > 0]] <- FALSE
    #dst <- spDists(this.xy, last.xy, longlat = TRUE)
    #ok.dst <- apply(dst, 1, function(x) min(x[x > 0]) < max.dist)
    last.xy <- xy[ok, ,drop = FALSE]
    points(last.xy, pch = 16)
    mean.point <- apply(last.xy, 2, mean)
    star.points <-  gcDestination(mean.point[1], mean.point[2], seq(0, 360, by = 6), dist = max.dist)
    for (n in 1:nrow(star.points)) {
        lines(rbind(mean.point, star.points[n,]))
    }
    #scan()
    mask2[,,i] <- ok
    if (nrow(last.xy) == nrow(this.xy)) break;
}



