#R

library(alphahull)

library(rgdal)

# Uniform sample of size n=300 in the annulus B(c,0.5)\B(c,0.25),

# with c=(0.5,0.5).

n <- 300

theta<-runif(n,0,2*pi)

r<-sqrt(runif(n,0.25^2,0.5^2))

x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta))

# Value of alpha

alpha <- 0.1

# alpha-shape

x.shape <- ashape(x, alpha = alpha)

## construct a GIS-poly object

all.coords <- matrix(t(x.shape$edges[, c(3:6)]), ncol = 2, byrow = TRUE)

coord <- all.coords[1,]
line.strings <- numeric(nrow(all.coords))
lstring <- 0
for (i in 2:nrow(all.coords)) {
	line.strings[i] <- lstring
	if (all(all.coords[i,] == coord)) {
		lstring <- lstring + 1
	}
}


polylist <- lapply(split(as.data.frame(all.coords), line.strings), function(x) Polygon(as.matrix(rbind(x, x[1,]))))


## one line object, one ID value

l <- list(Lines(l, as.character("1")))

## equivalent to a drawing with a single line object:

sldf <- SpatialLinesDataFrame(SpatialLines(l), data.frame(name = "ashape"), match.ID = FALSE)
writeOGR(sldf, ".", "lines", "ESRI Shapefile")



library(RODBC)

library(alphahull)



## define a function to connect to a Manifold file

odbcConnectManifold <- function (map.file)
{
    full.path <- function(filename) {
        fn <- chartr("\\", "/", filename)
        is.abs <- length(grep("^[A-Za-z]:|/", fn)) > 0
        chartr("/", "\\", if (!is.abs)
            file.path(getwd(), filename)
        else filename)
    }
    con <- if (missing(map.file))
        "Driver={Manifold Project Driver (*.map)};Dbq="
    else {
        fp <- full.path(map.file)
        paste("Driver={Manifold Project Driver (*.map)};DBQ=",
            fp, ";DefaultDir=", dirname(fp), ";Unicode=False;Ansi=False;OpenGIS=False;DSN=Default", ";", sep = "")
    }
    odbcDriverConnect(con)
}

## open a connection to the file

ch <- odbcConnectManifold("C:/temp/CreateBoundaryPointsCloud.map")

##sqlTables(ch)
xy <- sqlQuery(ch, "SELECT [X (I)] AS x, [Y (I)] AS y FROM [Points Table]")

x.hull <- ahull(x, alpha = .2)

mkarc <-
function (c, r, v, theta, segments = 30)
{
    angles <- anglesArc(v, theta)
    seqang <- seq(angles[1], angles[2], length = segments)
    x <- c[1] + r * cos(seqang)
    y <- c[2] + r * sin(seqang)
    cbind(x, y)
}


nsegs <- 30
arcs <- which(x.hull$arcs[, 3] > 0)

res.xy <- matrix(0, ncol = 2, nrow = nsegs * length(arcs))

    if (length(arcs) > 0) {
        for (i in 1:length(arcs)) {
	    istart <- (i-1) * nsegs + 1
            res.xy[istart:(istart + nsegs-1), ] <- mkarc(x.hull$arcs[arcs[i], 1:2], x.hull$arcs[arcs[i], 3], x.hull$arcs[arcs[i], 4:5],
                x.hull$arcs[arcs[i], 6], segments = nsegs)
        }
}


library(rgdal)
p <- SpatialPolygons(list(Polygons(list(Polygon(rbind(res.xy, res.xy[1,]), hole = FALSE)), "1")))
spdf <- SpatialPolygonsDataFrame(p, data.frame(name = "ahull30"))
writeOGR(spdf, ".", "ahull", "ESRI Shapefile")


library(RODBC)

library(alphahull)



## define a function to connect to a Manifold file

odbcConnectManifold <- function (map.file)
{
    full.path <- function(filename) {
        fn <- chartr("\\", "/", filename)
        is.abs <- length(grep("^[A-Za-z]:|/", fn)) > 0
        chartr("/", "\\", if (!is.abs)
            file.path(getwd(), filename)
        else filename)
    }
    con <- if (missing(map.file))
        "Driver={Manifold Project Driver (*.map)};Dbq="
    else {
        fp <- full.path(map.file)
        paste("Driver={Manifold Project Driver (*.map)};DBQ=",
            fp, ";DefaultDir=", dirname(fp), ";Unicode=False;Ansi=False;OpenGIS=False;DSN=Default", ";", sep = "")
    }
    odbcDriverConnect(con)
}

## open a connection to the file

ch <- odbcConnectManifold("C:/temp/CreateBoundaryPointsCloud.map")

##sqlTables(ch)
xy <- sqlQuery(ch, "SELECT [X (I)] AS x, [Y (I)] AS y FROM [Points Table]")

x.shape <- ashape(xy, alpha = 30)


res.xy <- matrix(0, ncol = 2, nrow = nrow(x.shape$edges) + 1)

for (i in 1:nrow(x.shape$edges)) {
    res.xy[i:(i+1), ] <- mkarc(x.hull$arcs[arcs[i], 1:2], x.hull$arcs[arcs[i], 3], x.hull$arcs[arcs[i], 4:5],
    x.hull$arcs[arcs[i], 6], segments = nsegs)
}


library(alphahull)
library(rgdal)

# Uniform sample of size n=300 in the annulus B(c,0.5)\B(c,0.25),
# with c=(0.5,0.5).
n <- 300
theta<-runif(n,0,2*pi)
r<-sqrt(runif(n,0.25^2,0.5^2))
x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta))




# Value of alpha
alpha <- 0.1
# alpha-shape
x.shape <- ashape(x, alpha = alpha)


## construct a GIS-ine object
l <- list()
for (i in 1:nrow(x.shape$edges)) {
	l[[i]] <-  Line(rbind(x.shape$edges[i, 3:4], x.shape$edges[i, 5:6]))
}

## one line object, one ID value
l <- list(Lines(l, as.character("1")))

## equivalent to a drawing with a single line object:

sldf <- SpatialLinesDataFrame(SpatialLines(l), data.frame(name = "ashape"), match.ID = FALSE)

writeOGR(sldf, ".", "lines", "ESRI Shapefile")

#################################################
Initial notes for OSGeo4W: RSB 090117-20

Run in OSGeo4W console, after setting:

set OSGEO4W_BUILD=yes
set GDAL_HOME=%OSGEO4W_ROOT%

cd rgdal/src

cl /MT /Ox /EHsc /D "WIN32" /c /I "C:/inst/R/R/include" /I C:/inst/OSGeo4W/include /I "C:\Program Files\Microsoft Visual Studio 9.0\VC\include" /D OSGEO4W *.cpp

link /dll /out:rgdal.dll /def:rgdal.def *.obj "C:\Program Files\R\R-2.8.1\bin\Rdll.lib" /libpath:"C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\lib" /libpath:"C:\Program Files\Microsoft SDKs\Windows\v6.0A\Lib" /libpath:"C:\OSGeo4W\lib" gdal_i.lib proj_i.lib

cd ../..

Set Makefile.win as below to all:!!

R CMD INSTALL --build rgdal

Post-installation, start R from the OSGeo4W console (command line). If you want
to check that the OSGeo4W-aware rgdal is present, check the reported support
file locations, and consider running:

source(system.file("OSGeo4W_test", package="rgdal"), echo=TRUE)

The initial version (0.6-6) is preliminary.



