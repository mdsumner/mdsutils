
## Grids and points already allow for n-dimensional data, and
## point-in-cell overlay, which is great. Plot, image, and write to
## file simply drop dimensions above 2. It seems to me that more value
## can be obtained by allowing lines and points the same capacity, but
## I need to explore more about what can go wrong.

## Otherwise, it seems neat if lines and polygons could accept any number
## of coordinate columns - otherwise we need special case classes for 3,
## 4 and up (maybe a special case for 3D or above, separate to 2D?).

## Line3D could simply be added as below, but does every level have to
## duplicated - SpatialLines/DataFrame and so on?

library(sp)
library(rgdal)

#########################################################
## POINTS AND GRIDS - LIMITED SUPPORT FOR N-D
## 3D/4D and higher rasters are supported in sp, and overlay works for
## nD points as well

nx <- 10
ny <- 7
nz <- 5
nt <- 4
x <- expand.grid(x = seq(0, 1, length = nx), y = seq(-2, 2, length = ny), z = seq(-1, 2, length = nz), t = seq(-6, 5, length = nt))

x$V1 <- 1:(prod(c(nx, ny, nz, nt)))
coordinates(x) <- ~x+y+z+t
gridded(x) <- TRUE

## write works, it just drops to 2D
writeGDAL(x, "out.tif")
summary(readGDAL("out.tif"))

## overlay works in nD
c4 <- cbind(runif(10, 0, 1), runif(10, -2, 2), runif(10, -1, 2), runif(10, -6, 5))
overlay(x, SpatialPoints(c4))




#################################################
## EXAMPLE 3D LINE

## set up a raster
data(volcano)
v <- volcano[20:40, 20:40] / 5
g <- image2Grid(list(x = 1:nrow(v), y = 1:ncol(v), z = v))

## simple set of coordinates on that raster
pts2 <- structure(c(13.2343050658723, 10.4414265950877, 9.66308341470515,
8.15218194690366, 6.32078622835639, 5.45087326204644, 4.26046604499072,
3.29898329275341, 3.25319839978973, 3.34476818571709, 3.89418690128127,
4.99302433240963, 5.95450708464694, 7.14491430170266, 8.42689130468574,
9.80043809359619, 10.7161359528698, 11.2197697754703, 12.3643920995624,
13.0969503869813, 14.4247122829280, 16.3934626803663, 18.1332886129862,
18.9574166863325, 19.3694807230056, 18.9574166863325, 18.9574166863325,
19.1863411511509, 19.4152656159693, 18.7742771144778, 17.0802360748216,
16.0271835366569, 15.3404101422017, 13.9210784603275, 13.5547993166181,
12.8680259221629, 1.49505393149506, 1.86133307520451, 2.09025754002292,
3.05174029226023, 4.69999643895277, 5.79883387008113, 8.0880785182652,
9.73633466495774, 10.4688929523766, 11.7966548483234, 12.7123527075970,
13.3991261020523, 13.7654052457617, 13.9943297105801, 14.0401146035438,
14.1774692824349, 14.4979635331806, 15.2763067135632, 17.3366268969289,
18.4354643280572, 19.3511621873309, 19.4427319732582, 18.8933132576941,
17.1992722180378, 15.0931671417085, 12.4834282427786, 10.4688929523766,
9.14113105642988, 7.17238065899157, 5.84461876304481, 5.11206047562591,
5.24941515451695, 5.52412451229904, 7.08081087306421, 7.99650873233784,
9.14113105642988), .Dim = c(36L, 2L))

## overlay to get the surface z
## ( offset slightly since the vertices are sampled only coarsely and will under/overlap the surface)
c3 <- cbind(pts2, z = overlay(g, SpatialPoints(pts2))$z + 2)

## break up the coordinates to generate lines, one with multi-parts
p1 <- c3[1:5, ]
p2 <- c3[9:15,]
p3 <- c3[17:25,]
p4 <- c3[27:36,]

l2 <- SpatialLines(list(
                        Lines(list(Line(p1[,1:2]),
                                   Line(p2[,1:2])), "1"),
                        Lines(list(Line(p3[,1:2])), "2"),
                        Lines(list(Line(p4[,1:2])), "3")
                        ))



l2.dat <- SpatialLinesDataFrame(l2, data.frame(a = 1:3), match.ID = FALSE)

matrix(unlist(lapply(obj@lines, function(x)
			lapply(x@Lines[1], function(x) coordinates(x)[1,]))), n, 2, byrow=TRUE)

#######################################################
## SIMPLISTIC EXTENSION FOR Line

## override the 2-column limitation
setClass("Line",
	representation(coords = "matrix"),
	prototype = list(coords = matrix(0)),
	validity = function(object) {
		if (any(is.na(object@coords)))
			stop("coords cannot contain missing values")
#		if (ncol(object@coords) != 2)
#			return("coords should have 2 columns")
#		if (nrow(object@coords) < 2)
#			return("Line should have at least 2 points")
		return(TRUE)
	}
)
Line <- function(coords) {
	coords <- coordinates(coords)
#	if (ncol(coords) != 2)
#		stop("coords must be a two-column matrix")
	new("Line", coords = coords)
}

bbox.default <- function(obj) {
	## is_points <- function(obj) {
	##     is <- FALSE
	##     if(is.array(obj))
	## 	if(length(dim(obj))==2)
	## 		if(dim(obj)[2]>=2) is <- TRUE
	##     is
	## }
	## if(!is_points(obj))stop('object not a >= 2-column array')


#	xr <- range(obj[,1],na.rm=TRUE)
#	yr <- range(obj[,2],na.rm=TRUE)
#	res <- rbind(x=xr, y=yr)
        res <-     t(apply(obj, 2, range))
	colnames(res) <- c("min","max")
	res
}



setMethod("bbox", "ANY", bbox.default)

setMethod("bbox", "Spatial", function(obj) obj@bbox)

bbox.Lines <- function(obj) {
	#rx=range(lapply(obj@Lines, function(x) range(x@coords[,1])))
	#ry=range(lapply(obj@Lines, function(x) range(x@coords[,2])))
	#res=rbind(x=rx,y=ry)
        res <- t(apply(do.call("rbind", lapply(obj@Lines, function(x) apply(x@coords, 2, range))), 2, range))

	dimnames(res)[[2]] <- c("min", "max")
	res
}

setMethod("bbox", "Lines", bbox.Lines)

bbox.Line <- function(obj) {
#    rx <- range(obj@coords[,1])
#    ry <- range(obj@coords[,2])
#	res = rbind(x = rx, y = ry)
    res <- t(apply(obj@coords, 2, range))

   	dimnames(res)[[2]] <- c("min", "max")
	res
}

setMethod("bbox", "Line", bbox.Line)

.bboxSls1 <- function(lst) {
#	bb = sapply(lst, bbox)
#	res = matrix(c(min(bb[1,]), min(bb[2,]), max(bb[3,]), max(bb[4,])), 2, 2)
#	dimnames(res) = list(c("x", "y"), c("min", "max"))
         bbox(t(do.call("cbind", lapply(lst, bbox))))


}


SpatialLines <- function(LinesList, proj4string=CRS(as.character(NA))) {
	if (any(sapply(LinesList, function(x) !is(x, "Lines"))))
		stop("lines list not exclusively filled with Lines objects")
	Sp <- new("Spatial", bbox = .bboxSls1(LinesList), proj4string=proj4string)
	res <- new("SpatialLines", Sp, lines=LinesList)
	res
}


spplot.polygons = function(obj, zcol = names(obj), ..., names.attr,
		scales = list(draw = FALSE), xlab = NULL, ylab = NULL, aspect = mapasp(obj,xlim,ylim),
		panel = panel.polygonsplot, sp.layout = NULL, formula,
		xlim = bbox(obj)[1,], ylim = bbox(obj)[2,]) {

    ## MDS
    require(lattice)
	if (is.null(zcol)) stop("no names method for object")
	sdf = as(obj, "data.frame")
	if (is(obj, "SpatialPolygonsDataFrame"))
		labpts = coordinates(obj)
	else {
		# get first points of each lines object:
		n = length(obj@lines)
                ## MDS
		labpts = matrix(unlist(lapply(obj@lines, function(x)
			lapply(x@Lines[1], function(x) coordinates(x)[1,]))), n, dimensions(obj), byrow=TRUE)[,1:2]
	}
	dimnames(labpts)[[2]] = c("xlabelpoint", "ylabelpoint")
	sdf = as.data.frame(cbind(labpts, sdf))
	coordinates(sdf) = c("xlabelpoint", "ylabelpoint")
	if (missing(formula))

            ## MDS
		formula = sp:::getFormulaLevelplot(sdf, zcol)
	if (length(zcol) > 1) {
		sdf = spmap.to.lev(sdf, zcol = zcol, names.attr = names.attr)
		zcol2 = "z"
	} else
		zcol2 = zcol
	if (is(obj, "SpatialPolygonsDataFrame"))
		grid.polygons = as(obj, "SpatialPolygons")
	else
		grid.polygons = as(obj, "SpatialLines")
    ## MDS
	scales = sp:::longlat.scales(obj, scales, xlim, ylim)

	args = append(list(formula, data = as(sdf, "data.frame"),
		aspect = aspect, grid.polygons = grid.polygons, panel =
		panel, xlab = xlab, ylab = ylab, scales = scales,
		sp.layout = sp.layout, xlim = xlim, ylim = ylim), list(...))
	if (all(unlist(lapply(obj@data[zcol], is.factor)))) {
		if (!is.null(args$col.regions) &&
				nlevels(obj@data[[zcol[1]]]) != length(args$col.regions))
			stop("length of col.regions should match number of factor levels")
		args$data[[zcol2]] = as.numeric(args$data[[zcol2]])
		if (is.null(args$colorkey) || (is.logical(args$colorkey) && args$colorkey)
				|| (is.list(args$colorkey) && is.null(args$colorkey$at) &&
					is.null(args$colorkey$labels))) {
			if (!is.list(args$colorkey))
				args$colorkey = list()
			ck = args$colorkey
			args$colorkey = NULL
			args = append(args, colorkey.factor(obj[[zcol[1]]], ck))
		} else
			args = append(args, colorkey.factor(obj[[zcol[1]]], ck, FALSE))
	}
	do.call("levelplot", args)
}

setMethod("spplot", signature("SpatialPolygonsDataFrame"), spplot.polygons)
setMethod("spplot", signature("SpatialLinesDataFrame"), spplot.polygons)
LineLength <-
function (cc, longlat = FALSE, sum = TRUE)
{
    if (is(cc, "Line"))
        cc = coordinates(cc)
    if (!is.matrix(cc))
        stop("cc must be a matrix")
    if (ncol(cc) != 2) warning("coordinates are > 2D, but only first two used")
##        stop("cc must have two columns")
    if (!is.numeric(cc))
        stop("cc must be numeric")
    x <- as.double(cc[, 1])
    y <- as.double(cc[, 2])
    n <- as.integer(length(x))
    if (n == 1)
        return(0)
    lengths <- vector(mode = "double", length = (n - 1))
    lonlat <- as.integer(longlat)
    res <- .C("sp_lengths", x, y, n, lengths, lonlat, PACKAGE = "sp")[[4]]
    if (any(!is.finite(res)))
        stop("non-finite line lengths")
    if (sum)
        res <- sum(res)
    res
}
LinesLength = function(Ls, longlat=FALSE) sum(sapply(Ls@Lines, LineLength, longlat))
SpatialLinesLengths <-
function (SL, longlat)
{
    if (missing(longlat)) {
        proj <- is.projected(SL)
        if (is.na(proj)) {
            longlat <- FALSE
        }
        else {
            longlat <- !proj
        }
    }
    if (!is.logical(longlat))
        stop("longlat should be logical")
    sapply(SL@lines, LinesLength, longlat = longlat)
}


getSpatialLinesMidPoints = function(SL) {
	ret = lapply(SL@lines,
		function(x) sapply(x@Lines,
			function(X) apply(X@coords, 2, mean)
		)
	)
	ret = t(sapply(ret, function(x) apply(x, 1, mean)))
	SpatialPoints(ret, CRS(proj4string(SL)))
}

## 3 lines, one with 2 parts
l3 <- SpatialLines(list(
                        Lines(list(Line(p1),
                                   Line(p2)), "1"),
                        Lines(list(Line(p3)), "2"),
                        Lines(list(Line(p4)), "3")
                        ))


l3.dat <- SpatialLinesDataFrame(l3, data.frame(a = 1:3), match.ID = FALSE)

## SpatialLines
## plot works as before
plot(l3, lwd = 5)
lines(l3, col = "green")

## why this warning? (not explored yet)
spplot(l3.dat)
#Warning message:
#In matrix(unlist(lapply(obj@lines, function(x) lapply(x@Lines[1],  :
#  data length [9] is not a sub-multiple or multiple of the number of columns [2]



## as does writeOGR
writeOGR(l3.dat, ".", "lines-Zdropped", "ESRI Shapefile")
summary(readOGR(".", "lines-Zdropped"))
## OGR data source with driver: ESRI Shapefile
## Source: ".", layer: "lines-Zdropped"
## with 1 features and 1 fields
## Feature type: wkbLineString with 2 dimensions
## Object of class SpatialLinesDataFrame
## Coordinates:
##        min      max
## x 3.253198 19.41527
## y 1.495054 19.44273
## Is projected: NA
## proj4string : [NA]
## Data attributes:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##       1       1       1       1       1       1



## the same goes for Polygons (not a serious example, it's topologically broken in 2D, let alone 3)

Polygon <- function (coords, hole = as.logical(NA))
{
    coords <- coordinates(coords)
   # if (ncol(coords) != 2)
   #     stop("coords must be a two-column matrix")
    n <- dim(coords)[1]
    stopifnot(is.logical(hole))
    ihole <- as.integer(hole)
    res <- .Call("Polygon_c", coords, n, ihole, PACKAGE = "sp")
    res
}


pp <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(rbind(c4[,], c4[1,]))), "1"))),
                               data.frame(x = 1), match.ID = FALSE)

plot(pp)
spplot(pp)

writeOGR(pp, ".", "pp-noZ", "ESRI Shapefile")
summary(readOGR("pp-noZ.shp", "pp-noZ"))
## OGR data source with driver: ESRI Shapefile
## Source: "pp-noZ.shp", layer: "pp-noZ"
## with 1 features and 1 fields
## Feature type: wkbPolygon with 2 dimensions
## Object of class SpatialPolygonsDataFrame
## Coordinates:
##          min      max
## x  0.1608752 0.988287
## y -1.8084622 1.168858
## Is projected: NA
## proj4string : [NA]
## Data attributes:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##       1       1       1       1       1       1



###
## an example of plotting in 3D geometry, this is pretty open-ended -
## but fairly trivially sensible here
## "3D lines" are not any less sensible in R than they are in OGR
## formats - 3D plotting is fairly easy with rgl, harder to support generally


library(rgl)
s3 <- as.image.SpatialGridDataFrame(g)
scl <- function(x) (x - min(x, na.rm = TRUE))/diff(range(x,  na.rm = TRUE))

surface3d(s3$x, s3$y, s3$z, col = terrain.colors(256)[scl(s3$z) * 255 + 1])
lines3d.SpatialLines <- function(x, col = "black", lwd = 1) {
    plotfun <- function(cds, ...) {
        lines3d(cds[,1], cds[,2], cds[,3], ...)
    }
    nlines <- length(x@lines)
    col <- rep(col, nlines)
    lwd <- rep(lwd, nlines)
    for (i in 1:nlines) {
       lapply(x@lines[[i]]@Lines, function(x1) plotfun(x1@coords, col = col[i], lwd = lwd[i]))
   }
    invisible(NULL)
}

lines3d.SpatialLines(l3, col = c("grey", "purple", "blue"), lwd = c(8, 4, 6))


## -sensible examples in 4D are very hard to arrange, since a tcltk slider (for example) would need
## control over continuous interpolation along the line for it to be slick, and that gets hard and expensive

### TESTING sp METHODS - rough notes
## bbox needed a lot of handling

bbox(l3)
coordinates(l3)
coordinates(l3@lines[[1]])
coordinates(l3@lines[[1]]@Lines[[1]])
row.names(l3) <- letters[1:3]
l3[1,]
coordnames(l3) <- c("x", "y", "z")
as(l3, "SpatialPointsDataFrame")
dimensions(l3.dat)

## error
LineLength(c4)
## error
SpatialLinesLengths(l3)


## interaction with bbox?

getSpatialLinesMidPoints(l3)

## asWKT limited to a mere print summary of the first coord?
## dimensions is limited by bbox
## spsample?
## surfaceArea for nD grids?
## summary limited by bbox

## rbind?
## recenter?
## chfids?
## polygons 2 lines?

## no
SpatialLinesLengths(l3)


## SpatialLinesDataFrame
l3.dat <- SpatialLinesDataFrame(l3, data.frame(a = 1:3), match.ID = FALSE)
plot(l3.dat)
lines(l3.dat)
## warning
spplot(l3.dat)
names(l3.dat)
as.data.frame(l3.dat)
row.names(l3.dat)
l3.dat[1,]
l3.dat$a

as(l3.dat, "SpatialPointsDataFrame")
split(l3.dat, 1:3)
print(l3.dat)



BayesX/R/extractSamples.R:    bayesxLog <- readLines(logfile)
BiodiversityR/inst/etc/BiodiversityGUI.R:            if (is.element(modelValue, listLinearModels())) {
BiodiversityR/inst/etc/BiodiversityGUI.R:            if (is.element(modelValue, listGeneralizedLinearModels())) {
BiodiversityR/inst/etc/BiodiversityGUI.R:        help(generalizedLinearModel)
BiodiversityR/inst/etc/BiodiversityGUI.R:            generalizedLinearModel()
BiodiversityR/inst/etc/BiodiversityGUI.R:            if (is.element(modelValue, listGeneralizedLinearModels())) {
BiodiversityR/inst/etc/BiodiversityGUI.R:        help(generalizedLinearModel)
CircSpatial/R/CircResidual.R:	circdist <- abs(raw - trend) # Linear distance in radians with NAs where raw has NAs
RSurvey/R/srvy.R:        txt <- paste(readLines(con, n=-1), collapse="\n")
RgoogleMaps/R/GetMap.R:	  ans <- readLines(n=1);
SDMTools/R/read.asc.R:		nc <- readLines(zz, 1); nc <- strsplit(nc, " "); nc <- as.numeric(nc[[1]][length(nc[[1]])])
SDMTools/R/read.asc.R:		nl <- readLines(zz, 1); nl <- strsplit(nl, " "); nl <- as.numeric(nl[[1]][length(nl[[1]])])
SDMTools/R/read.asc.R:		xll <- readLines(zz, 1); xll <- strsplit(xll, " ")
SDMTools/R/read.asc.R:		yll <- readLines(zz, 1); yll <- strsplit(yll, " ")
SDMTools/R/read.asc.R:		cs <- readLines(zz, 1); cs <- strsplit(cs, " "); cs <- as.numeric(cs[[1]][length(cs[[1]])])
SDMTools/R/read.asc.R:		nas <- readLines(zz, 1); nas <- strsplit(nas, " "); nas <- as.numeric(nas[[1]][length(nas[[1]])])
SQLiteMap/R/SQLiteMap.R:					plist[[1]] = Line(m)
SQLiteMap/R/SQLiteMap.R:				srs = Lines(plist, ID= ids[i])
SQLiteMap/R/SQLiteMap.R:						plist[[p]] = Line(m)
SQLiteMap/R/SQLiteMap.R:				srs = Lines(plist, ID= ids[i])
SQLiteMap/R/SQLiteMap.R:		SpP = SpatialLines(shps)
SQLiteMap/R/SQLiteMap.R:		if (obtyp == "SpatialLines")
SQLiteMap/R/SQLiteMap.R:		if (obtyp == "SpatialLinesDataFrame")
TSP/R/read_TSPLIB.R:    lines <- readLines(file)
UScensus2000add/R/demographics.add.R:geo<-readLines(paste(states.names[si],"/",state.ab[si],"geo.uf1",sep=""))
VIM/R/parcoordMiss.R:    localLines <- function(..., colcomb, xaxlabels) lines(...)
VIM/R/parcoordMiss.R:                localLines(xobs, szobs, col=col[3], lty=lty[3], xpd=xpd, ...)
VIM/R/parcoordMiss.R:            localLines(xobs, szobs, col=col[1], lty=lty[1], xpd=xpd, ...)
VIM/R/parcoordMiss.R:                localLines(xobs, szobs, col=col[4], lty=lty[4], xpd=xpd, ...)
VIM/R/parcoordMiss.R:            localLines(xobs, szobs, col=col[2], lty=lty[2], xpd=xpd, ...)
dismo/R/gbif.R:    x <- readLines(url, warn=FALSE)
dismo/R/gbif.R:		#s <- readLines(aurl, warn=FALSE)
dismo/R/maxent.R:		me@lambdas <- unlist( readLines( paste(dirout, '/species.lambdas', sep='') ) )
dismo/R/maxent.R:		html <- readLines(f)
dismo/R/maxent.R:		writeLines(html, f)
geoR/R/geoRmisc.R:    writeLines(strwrap(msg, prefix = "  "))
geoR/R/geoRmisc.R:    writeLines(strwrap(msg))
geosphere/R/perimeter.R:setMethod("perimeter", signature(x='SpatialLines'),
geosphere/R/perimeter.R:		parts = length( x[[i]]@Lines )
geosphere/R/perimeter.R:			crd = x[[i]]@Lines[[j]]@coords
gstat/demo/line.R:Sl = SpatialLines(list(Lines(list(Line(pp)), "a")))
gstat/demo/line.R:compare.krigingLines = function(formula, data, newdata, model) {
gstat/demo/line.R:compare.krigingLines(log(zinc)~1, meuse, Sl, v)
gstat/demo/line.R:Sl2 = SpatialLines(list(Lines(list(Line(pp),Line(pp2)), "a")))
gstat/demo/line.R:compare.krigingLines(log(zinc)~1, meuse, Sl2, v)
gstat/demo/line.R:Sl3 = SpatialLines(list(Lines(list(Line(pp)), "a"),Lines(list(Line(pp2)),"b")))
gstat/demo/wind.R:m = map2SpatialLines(
gstat/R/fit.variogram.gls.R:	    variogramLine(vgm(th[2], m, th[3], th[1]), dist_vector=h)$gamma
gstat/R/fit.variogram.gls.R:			variogramLine(model, dist_vector = dists[comb(i,i)])$gamma
gstat/R/fit.variogram.gls.R:			+ variogramLine(model, dist_vector = dists[comb(j,j)])$gamma
gstat/R/fit.variogram.gls.R:			- variogramLine(model, dist_vector = dists[comb(i,j)])$gamma
gstat/R/fit.variogram.gls.R:			- variogramLine(model, dist_vector = dists[comb(j,i)])$gamma,
gstat/R/gstat.formula.predict.R:	} else if (is(newdata, "SpatialLines")) {
gstat/R/gstat.formula.predict.R:		# locs = coordinates(getSpatialLinesMidPoints(newdata)) -- deprecated, now use:
gstat/R/gstat.formula.predict.R:	        	function(x) sapply(x@Lines,
gstat/R/gstat.formula.predict.R:		if (is(newdata, "SpatialLinesDataFrame"))
gstat/R/krige0.R:		V = variogramLine(model, dist_vector = spDists(s, s, ll),
gstat/R/krige0.R:		v0 = variogramLine(model, dist_vector = spDists(s, s0, ll),
gstat/R/krige0.R:		c0 = variogramLine(model, dist_vector = c(0), covariance=TRUE)$gamma
gstat/R/predict.gstat.R:	} else if (is(newdata, "SpatialLines")) {
gstat/R/predict.gstat.R:		nd = as(newdata, "SpatialLines")
gstat/R/predict.gstat.R:		} else if (is(newdata, "SpatialLines")) {
gstat/R/predict.gstat.R:			ret = SpatialLinesDataFrame(as(newdata, "SpatialLines"), ret,
gstat/R/show.vgms.R:			x = variogramLine(v, 0, 1, 0)
gstat/R/show.vgms.R:			x = variogramLine(v, max, n - 1, min)
gstat/R/show.vgms.R:			x = variogramLine(v, 0, 1, 0)
gstat/R/show.vgms.R:			x = variogramLine(v, max, n - 1, min)
gstat/R/variogramLine.R:# $Id: variogramLine.q,v 1.4 2008-08-18 16:32:42 edzer Exp $
gstat/R/variogramLine.R:"variogramLine" <-
gstat/R/variogramLine.R:		cat("variogram.line is DEPRECATED, please use variogramLine instead\n")
gstat/R/variogramLine.R:	variogramLine(...)
gstat/R/vgm.panel.R:            	ret <- variogramLine(model, max(x), dir = dir)
gstat/R/vgm.panel.R:					ret <- variogramLine(m[[id]], max(x), dir = dir)
gstat/R/vgm.panel.R:            	ret <- variogramLine(model, max = max(x), dir = dir)
gstat/tests/covtable.R:	variogramLine(vgm(1, "Sph", 1), 1, n=1e4,min = 0, covariance = TRUE))
gstat/tests/line.R:Sl = SpatialLines(list(Lines(list(Line(pp)), "a")))
gstat/tests/line.R:compare.krigingLines = function(formula, data, newdata, model) {
gstat/tests/line.R:compare.krigingLines(log(zinc)~1, meuse, Sl, v)
gstat/tests/line.R:Sl2 = SpatialLines(list(Lines(list(Line(pp),Line(pp2)), "a")))
gstat/tests/line.R:compare.krigingLines(log(zinc)~1, meuse, Sl2, v)
gstat/tests/line.R:Sl3 = SpatialLines(list(Lines(list(Line(pp)), "a"),Lines(list(Line(pp2)),"b")))
hydrosanity/R/gui_functions.R:guiTextInput <- function(text="", title="Text Input", prompt="", oneLiner=F,
hydrosanity/R/gui_functions.R:	size=c(600, 320), width.chars=-1, focus.on.ok=!oneLiner) {
hydrosanity/R/gui_functions.R:	if (oneLiner) {
hydrosanity/R/gui_functions.R:	newTxt <- if (oneLiner) editEntry['text'] else getTextviewText(editTV)
hydrosanity/R/hydrosanity_explore.R:	doDrawLine <- theWidget("explore_seasonal_drawline_radiobutton")$getActive()
hydrosanity/R/hydrosanity_explore.R:	plot.call$panel <- if (doDrawLine && !doSupStripPlot) {
hydrosanity/R/hydrosanity_explore.R:	} else if (doDrawLine && doSupStripPlot) {
hydrosanity/R/hydrosanity_explore.R:	plot.call$panel.groups <- if (doDrawLine && doSupStripPlot) {
hydrosanity/R/timeblob.R:		fileText <- readLines(file)
hydrosanity/R/timeblob.R:		firstLine <- read.table(file, header=F, skip=skip, sep=sep,
hydrosanity/R/timeblob.R:		fileCols <- ncol(firstLine)
hydrosanity/R/timeblob.R:		firstLine <- read.table(file, header=F, skip=skip, sep=sep,
hydrosanity/R/timeblob.R:		fileCols <- ncol(firstLine)
hydrosanity/R/timeblob.R:				if (is.numeric(i)) { firstLine[1,i] } else { i }
intamap/R/aggregation.R:      !is(predictionLocations,"SpatialLinesDataFrame") &
intamap/R/checkSetup.R:	     (is(object$predictionLocations,"SpatialLines") && length(object$predictionLocations@lines) > 2) ||
intamap/R/common.R:  } else if (is(spobj,"SpatialLines")) {
intamap/R/common.R:    SpatialLinesDataFrame(spobj,data = data,...)
intamap/R/init.R:  targetCRS,boundaries,boundaryLines,intCRS, params=list(),boundFile,lineFile,class="idw",
intamap/R/init.R:  if (!missing(boundaryLines)) {
intamap/R/init.R:    object$boundaryLines = boundaryLines
intamap/R/init.R:    object[[boundaryLines]] = lineFile
intamap/R/interpolate.R:		ret = readLines(file(fn, "r"))
intamap/R/yamamoto.R:  c0arr = variogramLine(model,dist_vector = c0dist)$gamma
intamap/R/yamamoto.R:  cmat = variogramLine(model, dist_vector = dvar)
intamap/R/yamamoto.R:    c0arr = variogramLine(model,dist_vector = c0dist)$gamma
intamap/R/yamamoto.R:    cmat = variogramLine(model,dist_vector = rvar)
intamap/tmp/aggregation.R:      !inherits(predictionLocations,"SpatialLinesDataFrame") &
intamap/tmp/bound.R:findBoundaryLines = function(regions, projOrig, projNew) {
intamap/tmp/bound.R:  boundaryLines = findBoundaries(regions)
intamap/tmp/bound.R:  if (!extends(class(boundaryLines),"Spatial")) coordinates(boundaryLines) = ~x+y
intamap/tmp/bound.R:  if (!missing(projNew) & (is.na(is.projected(boundaryLines)) |
intamap/tmp/bound.R:                          !is.projected(boundaryLines)))
intamap/tmp/bound.R:     proj4string(boundaryLines) = CRS(projNew)
intamap/tmp/bound.R:  return(boundaryLines)
intamap/tmp/findBiasUK.R:    V[,i]=variogramLine(vResFit$var_model,dist_vector=D[,i], debug.level = 0, covariance=T)[,2]
intamap/tmp/nutsData.R:  ans = readLines(con=stdin(),n = 1)
intamap/tmp/preProcess.R:  targetCRS,boundaries,boundaryLines,intCRS, params=list(),boundFile,lineFile,classes="idw",
intamap/tmp/preProcess.R:  if (!missing(boundaryLines)) {
intamap/tmp/preProcess.R:    object$boundaryLines = boundaryLines
intamap/tmp/preProcess.R:    object[[boundaryLines]] = lineFile
intamap/tmp/preProcess.R:          if ("boundaryLines" %in% names(object)){
intamap/tmp/preProcess.R:            boundaryLines = object$boundaryLines
intamap/tmp/preProcess.R:            boundaryLines = findBoundaryLines(regions=object$boundaries,
intamap/tmp/preProcess.R:                object$boundaryLines = boundaryLines
intamap/tmp/preProcess.R:          } else warning("No boundaryLines or boundaries in object")
intamap/tmp/preProcess.R:          if (exists("boundaryLines")) regionalBias = findRegionalBias(observations,boundaryLines,formulaString=formulaString)
intamap/tmp/preProcess.R:        if (exists("boundaryLines")) {
intamap/tmp/regBias.R:findRegionalBias = function(object,boundaryLines,formulaString=as.formula("value~1"),minKrige = 5) {
intamap/tmp/regBias.R:  cDiff = regionalDiff(object,regCode,gdat,boundaryLines,formulaString,minKrige=5)
intamap/tmp/regBias.R:regionalDiff = function(object,regCode,gdat,boundaryLines,formulaString,minKrige=5) {
intamap/tmp/regBias.R:    boundaries = boundaryLines
intamap/tmp/regBias.R:        xlinNew = SpatialLines(list(Lines(list(Line(coordinates(lbound))),ID = bord)))
maptools/R/asciigrid.R:	l5 = readLines(t, n = 6)
maptools/R/asciigrid.R:    	writeLines(c(paste("NCOLS", format(gp$cells.dim[1], decimal.mark=dec)),
maptools/R/DP.R: equationOfLine <- function(x1,x2,y1,y2) {
maptools/R/DP.R: 	line <- equationOfLine( points$x[start], points$x[end],
maptools/R/elide.R:    Lns <- slot(x, "Lines")
maptools/R/elide.R:      Line(new_crds)})
maptools/R/elide.R:    Lines(new_Lns, ID=slot(x, "ID"))})
maptools/R/elide.R:  res <- SpatialLines(new_lns)
maptools/R/elide.R:  res <- elide(as(obj, "SpatialLines"), bb=bb, shift=shift,
maptools/R/elide.R:  res <- SpatialLinesDataFrame(res, data=df)
maptools/R/elide.R:setMethod("elide", signature(obj="SpatialLines"), elide.lines)
maptools/R/elide.R:setMethod("elide", signature(obj="SpatialLinesDataFrame"), elide.linesdf)
maptools/R/getKMLcoordinates.R:    kml <- paste(readLines(kmlfile, encoding = "UTF-8"),
maptools/R/kmlLine.R:kmlLine <- function(obj = NULL, kmlfile = NULL, name = "R Line",
maptools/R/kmlLine.R:    if (class(obj) != "Lines" && class(obj) != "SpatialLinesDataFrame")
maptools/R/kmlLine.R:        stop("obj must be of class 'Lines' or 'SpatialLinesDataFrame' [package 'sp']")
maptools/R/kmlLine.R:    if (class(obj) == "SpatialLinesDataFrame") {
maptools/R/kmlLine.R:    kmlStyle <- append(kmlStyle, "<LineStyle>")
maptools/R/kmlLine.R:    kmlStyle <- append(kmlStyle, "</LineStyle>")
maptools/R/kmlLine.R:    for (i in 1:length(obj@Lines)) {
maptools/R/kmlLine.R:        kml <- append(kml, "<LineString>")
maptools/R/kmlLine.R:        kml <- append(kml, paste(coordinates(obj@Lines[[i]])[,
maptools/R/kmlLine.R:            1], coordinates(obj@Lines[[i]])[, 2], sep = ","))
maptools/R/kmlLine.R:        kml <- append(kml, "</LineString>")
maptools/R/kmlPolygon.R:    kmlStyle <- append(kmlStyle, "<LineStyle>")
maptools/R/kmlPolygon.R:    kmlStyle <- append(kmlStyle, "</LineStyle>")
maptools/R/kmlPolygon.R:        kml <- append(kml, c("<LinearRing>", "<coordinates>"))
maptools/R/kmlPolygon.R:        kml <- append(kml, c("</coordinates>", "</LinearRing>"))
maptools/R/readSplus.R:	lns<-readLines(file)
maptools/R/Rgshhs.R:              Ln <- Line(crds)
maptools/R/Rgshhs.R:              Lines(list(Ln), ID=ID)
maptools/R/Rgshhs.R:          res <- SpatialLines(Sll,
maptools/R/shapelib.R:    types <- c("Point", NA, "PolyLine", NA, "Polygon", NA, NA, "MultiPoint", NA, NA, "PointZ", NA, "PolyLineZ", NA, "PolygonZ", NA, NA, "MultiPointZ", NA, NA, "PointM", NA, "PolyLineM", NA, "PolygonM", NA, NA, "MultiPointM", NA, NA, "MultiPatch")
maptools/R/sp2Mondrian.R:                	writeLines(lab, con = con)
maptools/R/sp2Mondrian.R:	    writeLines(MAP_file, con, sep="")
maptools/R/sp2Mondrian.R:                	writeLines(lab, con = con)
maptools/R/sp2pbs.R:SpatialLines2PolySet <- function(SL) {
maptools/R/sp2pbs.R:		srs <- slot(pls[[i]], "Lines")
maptools/R/sp2pbs.R:PolySet2SpatialLines <- function(PS) {
maptools/R/sp2pbs.R:    outLines <- vector(mode="list", length=length(res0))
maptools/R/sp2pbs.R:        for (i in seq(along=outLines)) {
maptools/R/sp2pbs.R:            outLines[[i]] <- Lines(lapply(res1[[i]], function(x)
maptools/R/sp2pbs.R:                Line(cbind(x$X, x$Y))), ID=as.character(i))
maptools/R/sp2pbs.R:        for (i in seq(along=outLines)) {
maptools/R/sp2pbs.R:            outLines[[i]] <- Lines(lapply(res0[[i]], function(x)
maptools/R/sp2pbs.R:                Line(cbind(res0[[i]]$X, res0[[i]]$Y))), ID=as.character(i))
maptools/R/sp2pbs.R:    outSP <- SpatialLines(outLines, proj4string=CRS(p4s))
maptools/R/Spatial-methods.R:	types <- c("Point", NA, "PolyLine", NA, "Polygon", NA, NA,
maptools/R/Spatial-methods.R:	    "MultiPoint", NA, NA, "PointZ", NA, "PolyLineZ", NA,
maptools/R/Spatial-methods.R:	    "PolyLineM", NA, "PolygonM", NA, NA, "MultiPointM", NA, NA,
maptools/R/Spatial-methods.R:	} else if (typeSh == "PolyLine" || typeSh == "PolyLineZ") {
maptools/R/Spatial-methods.R:	    res <- readShapeLines(fn=fn, proj4string=proj4string,
maptools/R/Spatial-methods.R:	} else if (is(x, "SpatialLinesDataFrame")) {
maptools/R/Spatial-methods.R:	    writeLinesShape(x=x, fn=fn, factor2char=factor2char,
maptools/R/SpatialLines-methods.R:readShapeLines <- function(fn, proj4string=CRS(as.character(NA)),
maptools/R/SpatialLines-methods.R:	suppressWarnings(.shp2LinesDF(Map, proj4string=proj4string))
maptools/R/SpatialLines-methods.R:writeLinesShape <- function(x, fn, factor2char = TRUE, max_nchar=254) {
maptools/R/SpatialLines-methods.R:	pls <- .SpL2lineslist(as(x, "SpatialLines"))
maptools/R/SpatialLines-methods.R:.shp2LinesDF <- function(shp, proj4string=CRS(as.character(NA)), IDs) {
maptools/R/SpatialLines-methods.R:	LinesList <- vector(mode="list", length=n)
maptools/R/SpatialLines-methods.R:		LinesList[[i]] <- .shapes2LinesList(shapes[[i]], ID=IDs[i])
maptools/R/SpatialLines-methods.R:	SL <- SpatialLines(LinesList, proj4string=proj4string)
maptools/R/SpatialLines-methods.R:	res <- SpatialLinesDataFrame(SL, data=df)
maptools/R/SpatialLines-methods.R:.shapes2LinesList <- function(shape, ID) {
maptools/R/SpatialLines-methods.R:		res[[i]] <- Line(coords=shape$verts[from[i]:to[i],,drop=FALSE])
maptools/R/SpatialLines-methods.R:	Lines <- Lines(res, ID=ID)
maptools/R/SpatialLines-methods.R:	Lines
maptools/R/SpatialLines-methods.R:		xyL <- lapply(slot(pls[[i]], "Lines"),
maptools/R/spatstat1.R:as.psp.Line <- function(from, ..., window=NULL, marks=NULL, fatal) {
maptools/R/spatstat1.R:setAs("Line", "psp", function(from) as.psp.Line(from))
maptools/R/spatstat1.R:as.psp.Lines <- function(from, ..., window=NULL, marks=NULL, fatal) {
maptools/R/spatstat1.R:  y <- lapply(from@Lines, as.psp.Line, window=window)
maptools/R/spatstat1.R:setAs("Lines", "psp", function(from) as.psp.Lines(from))
maptools/R/spatstat1.R:as.psp.SpatialLines <- function(from, ..., window=NULL, marks=NULL, fatal) {
maptools/R/spatstat1.R:  y <- lapply(lin, as.psp.Lines, window=window)
maptools/R/spatstat1.R:setAs("SpatialLines", "psp", function(from) as.psp.SpatialLines(from))
maptools/R/spatstat1.R:as.psp.SpatialLinesDataFrame <- function(from, ..., window=NULL, marks=NULL, fatal) {
maptools/R/spatstat1.R:  y <- as(from, "SpatialLines")
maptools/R/spatstat1.R:    nseg.Line  <- function(x) { return(nrow(x@coords)-1) }
maptools/R/spatstat1.R:    nseg.Lines <- function(x) { return(sum(unlist(lapply(x@Lines, nseg.Line)))) }
maptools/R/spatstat1.R:    nrep <- unlist(lapply(y@lines, nseg.Lines))
maptools/R/spatstat1.R:setAs("SpatialLinesDataFrame", "psp", function(from) as.psp.SpatialLinesDataFrame(from))
maptools/R/spmaps.R:map2SpatialLines <- function(map, IDs=NULL, proj4string=CRS(as.character(NA))) {
maptools/R/spmaps.R:# assemble the list of Lines
maptools/R/spmaps.R:			if (nrow(crds) > 1) srl[[j]] <- Line(coords=crds)
maptools/R/spmaps.R:			else srl[[j]] <- Line(coords=rbind(crds, crds))
maptools/R/spmaps.R:		Srl[[i]] <- Lines(srl, ID=IDss[i])
maptools/R/spmaps.R:	res <- SpatialLines(Srl, proj4string=proj4string)
maptools/R/sp_bind.R:cbindSpatialLinesDataFrame <- function(obj, x) {
maptools/R/sp_bind.R:    SpatialLinesDataFrame(as(obj, "SpatialLines"), data=cx)
maptools/R/sp_bind.R:cbindSpatialLinesDataFramev <- function(obj, x) {
maptools/R/sp_bind.R:    SpatialLinesDataFrame(as(obj, "SpatialLines"), data=cx)
maptools/R/sp_bind.R:setMethod("spCbind", signature(obj="SpatialLinesDataFrame", x="data.frame"),
maptools/R/sp_bind.R:    cbindSpatialLinesDataFrame)
maptools/R/sp_bind.R:setMethod("spCbind", signature(obj="SpatialLinesDataFrame", x="vector"),
maptools/R/sp_bind.R:    cbindSpatialLinesDataFramev)
maptools/R/sp_bind.R:rbindSpatialLines <- function(obj, x) {
maptools/R/sp_bind.R:    SpatialLines(LL, proj4string=CRS(proj4string(obj)))
maptools/R/sp_bind.R:setMethod("spRbind", signature(obj="SpatialLines", x="SpatialLines"),
maptools/R/sp_bind.R:    rbindSpatialLines)
maptools/R/sp_bind.R:rbindSpatialLinesDataFrame <- function(obj, x) {
maptools/R/sp_bind.R:    SL <- spRbind(as(obj, "SpatialLines"), as(x, "SpatialLines"))
maptools/R/sp_bind.R:    SpatialLinesDataFrame(SL, data=df)
maptools/R/sp_bind.R:setMethod("spRbind", signature(obj="SpatialLinesDataFrame",
maptools/R/sp_bind.R:    x="SpatialLinesDataFrame"), rbindSpatialLinesDataFrame)
maptools/R/sp_tools.R:	hold <- readLines(con)
maptools/R/sp_tools.R:		res[[i]] <- Lines(list(Line(x
maptools/R/sp_tools.R:	SL <- SpatialLines(res, proj4string=proj4string)
maptools/R/sp_tools.R:	LinesList <- vector(mode="list", length=n)
maptools/R/sp_tools.R:		LinesList[[i]] <- Lines(list(Line(coords=crds
maptools/R/sp_tools.R:	SL <- SpatialLines(LinesList, proj4string=proj4string)
maptools/R/sp_tools.R:	res <- SpatialLinesDataFrame(SL, data=df)
maptools/R/sp_tools.R:ContourLines2SLDF <- function(cL, proj4string=CRS(as.character(NA))) {
maptools/R/sp_tools.R:		res[[i]] <- Lines(.contourLines2LineList(cL[cLstack[[i]]]#,
maptools/R/sp_tools.R:	SL <- SpatialLines(res, proj4string=proj4string)
maptools/R/sp_tools.R:	res <- SpatialLinesDataFrame(SL, data=df)
maptools/R/sp_tools.R:.contourLines2LineList <- function(cL#, proj4string=CRS(as.character(NA))
maptools/R/sp_tools.R:		res[[i]] <- Line(coords=crds#, proj4string=proj4string
nnDiag/R/arealBias.R:  smartlegend(x="left", y="top", c("2-SE Interval","1:1 Line"), lty=c(2,1), bty="n")
nnDiag/R/bias.R:    smartlegend(x="left", y="top", "1:1 Line", inset = 0.02, lty=2, bty="n")
nnDiag/R/bias.R:    smartlegend(x="left", y="top", "1:1 Line", inset = 0.005, lty=2, bty="n")
playwith/demo/clusterApp.R:                annLine <- call("panel.abline", h = height,
playwith/demo/clusterApp.R:                playAnnotate(playState, annLine, add = FALSE,
playwith/demo/lineEqTool.R:    lnInfo <- playLineInput(playState)
playwith/demo/lineEqTool.R:lineEqTool <- list("LineEq", "gtk-indent", "Line + Eqn",
playwith/R/apiAnnotation.R:playLineInput <-
playwith/R/apiAnnotation.R:    gcb$setLineAttributes(line.width=1, line.style=GdkLineStyle["solid"],
playwith/R/apiAnnotation.R:    gc$setLineAttributes(line.width=1, line.style=GdkLineStyle["double-dash"],
playwith/R/apiAnnotation.R:               line = gdkDrawLine(event$window, gc=tmp.gc,
playwith/R/apiMain.R:    writeLines(txt[inds], file2)
playwith/R/apiMain.R:    else paste("$", deparseOneLine(arg), sep="")
playwith/R/apiMain.R:    else paste("$", deparseOneLine(arg), sep="")
playwith/R/autoplay.R:                                        toString(deparseOneLine(x), width=34)))
playwith/R/autoplay.R:                                        toString(deparseOneLine(x), width=34)))
playwith/R/playwith.R:        callTxt <- deparseOneLine(playState$call, control=
playwith/R/playwith.R:        callText <- toString(deparseOneLine(conditionCall(e)),
playwith/R/playwith.R:deparseOneLine <-
playwith/R/plotSettingsGui.R:            argVal <- paste(sapply(argVal, deparseOneLine), collapse="; ")
playwith/R/plotSettingsGui.R:            svalue(wid$index.cond) <- deparseOneLine(val)
playwith/R/plotSettingsGui.R:            argVal <- paste(sapply(argVal, deparseOneLine), collapse="; ")
playwith/R/plotSettingsGui.R:    hasLines <- any(c("l","b","o") %in% arg_type)
playwith/R/plotSettingsGui.R:    wid$lines <- gcheckbox("Lines", checked=hasLines, container = tmp,
playwith/R/uiAnnotationActions.R:    lay[2,1] <- "Lineheight:"
playwith/R/uiClickActions.R:                  Line = "Drag to draw a line (hold Shift to constrain)",
playwith/R/uiClickActions.R:    pageOK <- (modeOK %in% c("Annotation", "Arrow", "Line", "Rect"))
playwith/R/uiClickActions.R:            if (modeOK %in% c("Pan", "Rotate", "Arrow", "Line"))
playwith/R/uiClickActions.R:            if (modeOK == "Line") {
playwith/R/uiGlobalActions.R:    callTxt <- deparseOneLine(playState$call, control=
playwith/R/uiIdentifyActions.R:    rnCode <- deparseOneLine(call("rownames", datArg))
playwith/R/uiPlotActions.R:             list("SetPointLineStyle", NULL, "Set _point/line style...", "<Alt>1", NULL, set.point.line.style_handler),
playwith/R/uiPlotActions.R:             list("StyleThickLines", NULL, "Thick lines", NULL, NULL, style.thick.lines_handler, FALSE),
playwith/R/uiPlotActions.R:             list("Line", NULL, "_Line", "<Alt>bar", "Add lines to the plot", 6),
playwith/R/uiPlotActions.R:         "Line" = 6,
playwith/R/uiStyleActions.R:                              oneLiner = TRUE, width.chars = 7)
playwith/R/uiStyleActions.R:    glabel(" Line Width:", container = tmp3g)
playwith/R/uiStyleActions.R:    glabel(" Line Type:", container = tmp3g)
playwith/R/uiStyleActions.R:    glabel(" Line height factor:", container = tmp2g)
playwith/R/utilities.R:             oneLiner=FALSE,
playwith/R/utilities.R:             focus.on.ok=!oneLiner)
playwith/R/utilities.R:    if (oneLiner) {
playwith/R/utilities.R:    newTxt <- if (oneLiner) editEntry["text"] else getTextviewText(editTV)
raster/R/contour.R:	cL <- contourLines(x=xFromCol(x,1:ncol(x)), y=yFromRow(x, nrow(x):1), z=t((getValues(x, format='matrix'))[nrow(x):1,]), ...)
raster/R/contour.R:# The below was taken from ContourLines2SLDF(maptools), by Roger Bivand & Edzer Pebesma
raster/R/contour.R:	.contourLines2LineList <- function(cL) {
raster/R/contour.R:			res[[i]] <- Line(coords=crds)
raster/R/contour.R:        res[[i]] <- Lines(.contourLines2LineList(cL[cLstack[[i]]]), ID = IDs[i])
raster/R/contour.R:    SL <- SpatialLines(res, proj4string = projection(x, asText=FALSE))
raster/R/contour.R:    SpatialLinesDataFrame(SL, data = df)
raster/R/drawPoly.R:drawLine <- function(sp=TRUE, col='red', lwd=2, ...) {
raster/R/drawPoly.R:		return( SpatialLines(list(Lines(list(Line(xy)), "1"))) )
raster/R/getData.R:			x <- readLines(h)
raster/R/getData.R:			writeLines(x, h)
raster/R/inifile.R:	Lines <- readLines(filename,  warn = FALSE)
raster/R/inifile.R:	Lines <- trim(Lines)
raster/R/inifile.R:	ini <- lapply(Lines, strspcom)
raster/R/inifile.R:	Lines <- matrix(unlist(ini), ncol=2, byrow=TRUE)[,1]
raster/R/inifile.R:	ini <- lapply(Lines, strsp)
raster/R/linesToRaster.R:linesToRaster <- function(spLines, raster, field=0, filename="", updateRaster=FALSE, updateValue="NA", ...) {
raster/R/linesToRaster.R:	if (projection(spLines) != "NA") {
raster/R/linesToRaster.R:		projection(raster) = projection(spLines)
raster/R/linesToRaster.R:	if (class(spLines) == 'SpatialPolygons') {
raster/R/linesToRaster.R:		spLines <- as(spLines, "SpatialLines")
raster/R/linesToRaster.R:	if (class(spLines) == 'SpatialPolygonsDataFrame') {
raster/R/linesToRaster.R:		spLines <- as(spLines, "SpatialLinesDataFrame")
raster/R/linesToRaster.R:	if (!(class(spLines) == 'SpatialLines' | class(spLines) == 'SpatialLinesDataFrame')) {
raster/R/linesToRaster.R:		stop('spLines should be a SpatialLines* object')
raster/R/linesToRaster.R:# check if bbox of raster and spLines overlap
raster/R/linesToRaster.R:	spbb <- bbox(spLines)
raster/R/linesToRaster.R:	nline <- length(spLines@lines)
raster/R/linesToRaster.R:		info[i,1] <- length(spLines@lines[[i]]@Lines)
raster/R/linesToRaster.R:			miny <- min(miny, min(spLines@lines[[i]]@Lines[[j]]@coords[,2]))
raster/R/linesToRaster.R:			maxy <- max(maxy, max(spLines@lines[[i]]@Lines[[j]]@coords[,2]))
raster/R/linesToRaster.R:	if (class(spLines) == 'SpatialLines' | field == 0) {
raster/R/linesToRaster.R:		putvals <- as.vector(spLines@data[,field])
raster/R/linesToRaster.R:					if ( max ( spLines@lines[[i]]@Lines[[j]]@coords[,2] ) < lly  |  min( spLines@lines[[i]]@Lines[[j]]@coords[,2] ) > uly ) {
raster/R/linesToRaster.R:						aline <- spLines@lines[[i]]@Lines[[j]]@coords
raster/R/polygonToRaster.R:.intersectLinePolygon <- function(line, poly) {
raster/R/polygonToRaster.R:				intersection <- .intersectLinePolygon(myline, mypoly@coords)
raster/R/polygonToRaster.R:					intersection <- .intersectLinePolygon(myline, mypoly@coords)
raster/R/rasterFromASCII.R:	lines <- readLines(con, n=6)
raster/R/saveOptions.R:	lst <- readLines(fn)
raster/R/saveOptions.R:	lst <- readLines(fn)
raster/R/writeFormats.R:	return(  c("R-raster", "SAGA GIS", "IDRISI", "Band by Line", "Band Sequential", "Band by Pixel") )
rgdal/R/make_EPSG.R:		res <- strsplit(paste(readLines(epsg, n=2), collapse=""),
rgdal/R/make_EPSG.R:		if (is.na(code[i])) res <- readLines(epsg, n=1)
rgdal/R/ogr.R:  WKB <- c("wkbPoint", "wkbLineString", "wkbPolygon", "wkbMultiPoint",
rgdal/R/ogr.R:    "wkbMultiLineString", "wkbMultiPolygon", "wkbGeometryCollection")
rgdal/R/ogr_sp.R:	WKB <- c("wkbPoint", "wkbLineString", "wkbPolygon", "wkbMultiPoint",
rgdal/R/ogr_sp.R:	    "wkbMultiLineString", "wkbMultiPolygon", "wkbGeometryCollection")
rgdal/R/ogr_sp.R:				lnlist[[j]] <- Line(cbind(jG[[1]], jG[[2]]))
rgdal/R/ogr_sp.R:			lnList[[i]] <- Lines(lnlist, ID=as.character(fids[i]))
rgdal/R/ogr_sp.R:		SL <- SpatialLines(lnList, proj4string=CRS(p4s))
rgdal/R/ogr_sp.R:		res <- SpatialLinesDataFrame(SL, data)
rgdal/R/project.R:".spTransform_Line" <- function(x, to_args, from_args, ii, jj) {
rgdal/R/project.R:		stop(paste("failure in Lines", ii, "Line", jj,
rgdal/R/project.R:	x <- Line(coords=crds)
rgdal/R/project.R:".spTransform_Lines" <- function(x, to_args, from_args, ii) {
rgdal/R/project.R:	input <- slot(x, "Lines")
rgdal/R/project.R:	for (i in 1:n) output[[i]] <- .spTransform_Line(input[[i]],
rgdal/R/project.R:	x <- Lines(output, ID)
rgdal/R/project.R:"spTransform.SpatialLines" <- function(x, CRSobj, ...) {
rgdal/R/project.R:	for (i in 1:n) output[[i]] <- .spTransform_Lines(input[[i]],
rgdal/R/project.R:	res <- SpatialLines(output, proj4string=CRS(to_args))
rgdal/R/project.R:setMethod("spTransform", signature("SpatialLines", "CRS"), spTransform.SpatialLines)
rgdal/R/project.R:"spTransform.SpatialLinesDataFrame" <- function(x, CRSobj, ...) {
rgdal/R/project.R:	xSP <- as(x, "SpatialLines")
rgdal/R/project.R:	res <- SpatialLinesDataFrame(sl=resSP, data=xDF, match.ID = FALSE)
rgdal/R/project.R:setMethod("spTransform", signature("SpatialLinesDataFrame", "CRS"), spTransform.SpatialLinesDataFrame)
spdep/R/nb2lines.R:				Ll <- list(Line(xy))
spdep/R/nb2lines.R:				ll[[line]] <- Lines(Ll, ID=as.character(line))
spdep/R/nb2lines.R:	SpatialLinesDataFrame(SpatialLines(ll, proj4string=proj4string),
spdep/R/read.gal.R:	line <- unlist(strsplit(readLines(con, 1), " "))
spdep/R/read.gal.R:		line <- unlist(strsplit(readLines(con, 1), " "))
spdep/R/read.gal.R:		line <- unlist(strsplit(readLines(con, 1), " "))
spdep/R/read.gal.R:	if (oldstyle) writeLines(paste(n), con)
spdep/R/read.gal.R:	else writeLines(paste("0", n, shpfile, ind, sep=" "), con)
spdep/R/read.gal.R:			writeLines(paste(i, cn[i],
spdep/R/read.gal.R:		else writeLines(paste(rn[i], cn[i],
spdep/R/read.gal.R:		if (oldstyle) writeLines(ifelse(cn[i] > 0,
spdep/R/read.gal.R:		else writeLines(ifelse(cn[i] > 0,
spdep/R/read.gwt2nb.R:	firstline <- unlist(strsplit(readLines(con,1)," "))
spdep/R/read.gwt2nb.R:	if (legacy) writeLines(format(n), con)
spdep/R/read.gwt2nb.R:        else writeLines(paste("0", n, shpfile, ind, sep=" "), con)
spdep/R/read.gwt2nb.R:	writeLines(field, con)
spgrass6/R/bin_link.R:	l8 <- readLines(con, n=8)
spgrass6/R/bin_link.R:	l6 <- readLines(con, n=6)
spgrass6/R/bin_link.R:	writeLines(paste("nrows", grd@cells.dim[2]), f)
spgrass6/R/bin_link.R:	writeLines(paste("ncols", grd@cells.dim[1]), f)
spgrass6/R/bin_link.R:	writeLines(paste("nbands 1"), f)
spgrass6/R/bin_link.R:	writeLines(paste("nbits", 8*sz), f)
spgrass6/R/bin_link.R:	writeLines(paste("byteorder", ifelse(.Platform$endian == "little",
spgrass6/R/bin_link.R:	writeLines(paste("layout bil"), f)
spgrass6/R/bin_link.R:	writeLines(paste("skipbytes 0"), f)
spgrass6/R/bin_link.R:	writeLines(paste("nodata", na.value), f)
spgrass6/R/bin_link.R:	writeLines(formatC(grd@cellsize[1], format="f"), f)
spgrass6/R/bin_link.R:	writeLines("0.0", f)
spgrass6/R/bin_link.R:	writeLines("0.0", f)
spgrass6/R/bin_link.R:	writeLines(formatC(-grd@cellsize[2], format="f"), f)
spgrass6/R/bin_link.R:	writeLines(formatC(grd@cellcentre.offset[1], format="f"), f)
spgrass6/R/bin_link.R:	writeLines(formatC((grd@cellcentre.offset[2] +
spgrass6/R/vect_link.R:						plijp <- slot(plij, "Lines")
spgrass6/R/vect_link.R:				    npls[[i]] <- Lines(srl, ID=IDss[i])
spgrass6/R/vect_link.R:			    SP <- SpatialLines(npls, proj4string=CRS(p4s))
spgrass6/R/vect_link.R:			    res <- SpatialLinesDataFrame(SP, ndata)
spgrass6/R/vect_link.R:	if (class(SDF) == "SpatialLinesDataFrame") type <- "line"
spsurvey/demo/Linear.R:# Demo: Linear
spsurvey/demo/Linear.R:# Purpose: Example GRTS Survey Designs for a Linear Resource
yaImpute/R/asciigridimpute.R:      newhead = readLines(infh[[i]],n=6)


adehabitat/R/dunnfa.r:        nf <- as.integer(readLines(n = 1))
adehabitat/R/enfa.r:        nf <- as.integer(readLines(n = 1))
adehabitat/R/export.asc.r:    writeLines(nc, zz)
adehabitat/R/export.asc.r:    writeLines(nl, zz)
adehabitat/R/export.asc.r:    writeLines(xll, zz)
adehabitat/R/export.asc.r:    writeLines(yll, zz)
adehabitat/R/export.asc.r:    writeLines(cs, zz)
adehabitat/R/export.asc.r:    writeLines(nas, zz)
adehabitat/R/getverticeshr.r:          re <- contourLines(x = xyl$x,
adehabitat/R/gnesfa.r:      nfFirst <- as.integer(readLines(n = 1))
adehabitat/R/gnesfa.r:      nfLast <- as.integer(readLines(n = 1))
adehabitat/R/import.asc.r:    nc <- readLines(zz, 1)
adehabitat/R/import.asc.r:    nl <- readLines(zz, 1)
adehabitat/R/import.asc.r:    xll <- readLines(zz, 1)
adehabitat/R/import.asc.r:    yll <- readLines(zz, 1)
adehabitat/R/import.asc.r:    cs <- readLines(zz, 1)
adehabitat/R/import.asc.r:    nas <- readLines(zz, 1)
adehabitat/R/import.asc.r:    tmp <- readLines(zz)
adehabitat/R/import.asc.r:    writeLines(tmp, zz)
adehabitat/R/kernelkc.r:        re <- contourLines(x = xyl$x,
adehabitat/R/kernelkc.r:    re <- contourLines(x = xyl$x,
adehabitat/R/ltraj2sldf.r:                   function(x) Line(as.matrix(x[!is.na(x$x),c("x","y")])))
adehabitat/R/ltraj2sldf.r:        re1 <- lapply(lev, function(x) Lines(lixy[id==x], ID=x))
adehabitat/R/ltraj2sldf.r:        res <- SpatialLines(re1)
adehabitat/R/ltraj2sldf.r:                      function(i) Lines(list(lixy[[i]]), ID=bu[i]))
adehabitat/R/ltraj2sldf.r:        res <- SpatialLines(res)
adehabitat/R/ltraj2sldf.r:    res <- SpatialLinesDataFrame(res, data=df)
adehabitat/R/madifa.r:        nf <- as.integer(readLines(n = 1))
adehabitat/R/traj2sldf.r:    ## Conversion as list of Lines
adehabitat/R/traj2sldf.r:                   function(x) Line(as.matrix(x)))
adehabitat/R/traj2sldf.r:        ## If one line per ID -> SpatialLines
adehabitat/R/traj2sldf.r:        re1 <- lapply(lev, function(x) Lines(lixy[id==x], ID=x))
adehabitat/R/traj2sldf.r:        res <- SpatialLines(re1)
adehabitat/R/traj2sldf.r:        ## If one line per ID -> SpatialLines
adehabitat/R/traj2sldf.r:                      function(i) Lines(list(lixy[[i]]), ID=names(lixy)[i]))
adehabitat/R/traj2sldf.r:        res <- SpatialLines(res)
adehabitat/R/traj2sldf.r:    res <- SpatialLinesDataFrame(res, data=df)
adehabitat/R/trajdyn.r:                      "r/l            -- add or remove points/Lines",
pgirmess/R/readVista.r:        concat<-readLines(tmp,warn=FALSE)
pgirmess/R/readVista.r:        x<-readLines(tmp,warn=FALSE)
r2dRue/R/r2dRue.r:              #num de bloques de size LinesToRead en la imagen
r2dRue/R/r2dRue.r:      #num de bloques de size LinesToRead en la imagen
spsurvey/R/read.shape.r:#   assigned class "SpatialPointsDataFrame", "SpatialLinesDataFrame", or
spsurvey/R/read.shape.r:#   shape2spList - function to create an object of class Lines for a lines
spsurvey/R/read.shape.r:#   SpatialLines - sp package function to create an object of class SpatialLines
spsurvey/R/read.shape.r:#   SpatialLinesDataFrame - sp package function to create an object of class
spsurvey/R/read.shape.r:#     SpatialLinesDataFrame
spsurvey/R/read.shape.r:      sp.obj <- SpatialLinesDataFrame(sl=SpatialLines(LinesList=SlinesList),
spsurvey/R/shape2spList.r:#   This function creates an object of class Lines for a Polyline shapefile or
spsurvey/R/shape2spList.r:#   Line - sp package function to create an object of class Line
spsurvey/R/shape2spList.r:#   Lines - sp package function to create an object of class Lines
spsurvey/R/shape2spList.r:         temp[[i]] <- Line(coords=shape$verts[from[i]:to[i],])
spsurvey/R/shape2spList.r:      Lines <- Lines(slinelist=temp, ID=ID)
spsurvey/R/shape2spList.r:      return(Lines)
spsurvey/R/sp2shape.r:#   "SpatialLinesDataFrame", or "SpatialPolygonsDataFrame".
spsurvey/R/sp2shape.r:   } else if(class(sp.obj) == "SpatialLinesDataFrame") {
spsurvey/R/sp2shape.r:         nparts[i] <- length(sp.obj@lines[[i]]@Lines)
spsurvey/R/sp2shape.r:               nextpart <- nrow(sp.obj@lines[[i]]@Lines[[j]]@coords)
spsurvey/R/sp2shape.r:                  nrow(sp.obj@lines[[i]]@Lines[[j]]@coords)
spsurvey/R/sp2shape.r:            xcoord <- c(xcoord, sp.obj@lines[[i]]@Lines[[j]]@coords[,1])
spsurvey/R/sp2shape.r:            ycoord <- c(ycoord, sp.obj@lines[[i]]@Lines[[j]]@coords[,2])
