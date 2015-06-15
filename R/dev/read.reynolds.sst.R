read.reynolds.sst <- function(xlim = NULL, ylim = NULL, tlim = NULL, filename = "G:/DATA/Reynolds/sst.wkmean.1990-present.nc") {

    xlim <- c(100, 180)
    ylim <- c(-80, 0)
    tlim <- ISOdatetime(2008:2009, 1, 1, 0, 0, 0)
    stopifnot(require(RNetCDF))
    stopifnot(file.exists(filename))

    ## TODO try catch
    nc <- open.nc(filename)
    xx <- var.get.nc(nc, "lon")
    yy <- rev(var.get.nc(nc, "lat"))

    tt <- var.get.nc(nc, "time")

    tt <- as.POSIXct("1800-1-1 00:00:00", tz = "GMT") + tt * 24 * 3600
    if (is.null(xlim)) xlim <- range(xx)
    if (is.null(ylim)) ylim <- range(yy)
    if (is.null(tlim)) tlim <- range(tt)

    xi <-  findInterval(xlim, xx)
    yi <- length(yy) - findInterval(ylim, yy) + 1
    ti <-  findInterval(tlim, tt)

    xsub <- seq(xi[1], xi[2])
    ysub <- seq(yi[2], yi[1])
    tsub <- seq(ti[1], ti[2])

    res <- list(x = xx[xsub], y = yy[ysub], t = tt[tsub], sst = var.get.nc(nc, "sst", start = c(xi[1], yi[2], ti[1]), count = c(length(xsub), length(ysub), length(tsub))))


res
}
