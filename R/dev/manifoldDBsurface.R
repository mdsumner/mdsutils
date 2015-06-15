library(RODBC)

## define a function to connect to a Manifold file
## (see ?odbcConnectExcel for another example)


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

setwd("E:/DATA/Etopo")

ch <- odbcConnectManifold("Etpopo2Ice.map")

xlim <- c(130, 160)
ylim <- c(-50, -30)
offset <- c(-180, -90)
cdim <- c(1000, 500)
csize <- c(360, 180)/cdim

xs <- seq(offset[1], by = csize[1], length = cdim[1])
wx <- which(xs >= xlim[1] & xs  <= xlim[2])
xindexlim <- range(wx-1)
ys <- seq(offset[2], by = csize[2], length = cdim[2])
wy <- which(ys >= ylim[1] & ys  <= ylim[2])
yindexlim <- range(wy-1)

qu <- paste("SELECT [Elevation] FROM [Etopo2Ice Table] WHERE ([X Index] >= ",
            xindexlim[1], " AND [X Index] <= ", xindexlim[2],
            ") AND ([Y Index] >= ",
            yindexlim[1], " AND [Y Index] <= ", yindexlim[2], ");")

d <- sqlQuery(ch, qu)

close(ch)


## this is slow, could be improved by doing the sort in the DB
#d$X <- (d$GridIndex %% 10800) * 360/10800 + -180
#d$Y <- 90- (d$GridIndex %/% 21600) * 360/10800
# gridded(d) <- ~X+Y

 library(tripEstimation)
l <- list(x = xs[wx], y = ys[wy], z = matrix(d$Elevation, length(wx), length(wy))[,length(wy):1])
image(mkSmall(l))



