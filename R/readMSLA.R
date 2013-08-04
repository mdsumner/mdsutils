#' Read Southern Ocean Mean Sea Level Anomaly (MSLA) data.
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param xlim %% ~~Describe \code{xlim} here~~
#' @param ylim %% ~~Describe \code{ylim} here~~
#' @param tlim %% ~~Describe \code{tlim} here~~
#' @param mslafile %% ~~Describe \code{mslafile} here~~
#' @param sofile %% ~~Describe \code{sofile} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function(xlim, ylim, tlim, mslafile = "tp_ers_msla_southern_ocean.nc", sofile = "E:\DATA\msla\SOfronts.Rdata") {
#'     require(RNetCDF)
#'     if (!file.exists(mslafile)) stop("no file ", mslafile)
#'     if (!file.exists(sofile)) stop("no file ", sofile)
#'     load(sofile)
#' 
#'     nc <- open.nc(mslafile)
#'     xx <- var.get.nc(nc, "lon")
#'     yy <- var.get.nc(nc, "lat")
#'     tt <- var.get.nc(nc, "time") * 24 * 3600 + as.POSIXct("1992-10-14 00:00:00", tz = "GMT")
#' 
#'     #int time(time) ;
#'     #time:long_name = "central time of +/- 10-15 day window" ;
#'     #time:units = "days since 1992-10-14 0:0:0.0" ;
#'     #time:first_date = "14-OCT-1992" ;
#' 
#'     xind <- which(xx >= xlim[1] & xx <= xlim[2])
#'     yind <- which(yy >= ylim[1] & yy <= ylim[2])
#'     tind <- which(tt >= tlim[1] & tt <= tlim[2])
#'     start <- c(min(xind), min(yind), min(tind))
#'     count <- c(length(xind), length(yind), length(tind))
#'     res <- list(x = xx[xind], y = yy[yind], height = var.get.nc(nc, "height", start, count))
#'     close.nc(nc)
#' 
#'     if (length(dim(res$height)) > 2) {
#'         for (i in 1:dim(res$height)[3]) res$height[,,i] <- res$height[,,i] + t(SOfronts$HOlb[yind, xind][,-dim(SOfronts$HOlb)[2]])/10
#'     } else {
#'         res$height <- res$height + t(SOfronts$HOlb[yind, xind][,-dim(SOfronts$HOlb)[2]])/10
#'     }
#'     res
#'   }
#' 
readMSLA <-
function(xlim, ylim, tlim, mslafile = "tp_ers_msla_southern_ocean.nc", sofile = "E:\\DATA\\msla\\SOfronts.Rdata") {
    require(RNetCDF)
    if (!file.exists(mslafile)) stop("no file ", mslafile)
    if (!file.exists(sofile)) stop("no file ", sofile)
    load(sofile)

    nc <- open.nc(mslafile)
    xx <- var.get.nc(nc, "lon")
    yy <- var.get.nc(nc, "lat")
    tt <- var.get.nc(nc, "time") * 24 * 3600 + as.POSIXct("1992-10-14 00:00:00", tz = "GMT")

    #int time(time) ;
    #time:long_name = "central time of +/- 10-15 day window" ;
    #time:units = "days since 1992-10-14 0:0:0.0" ;
    #time:first_date = "14-OCT-1992" ;

    xind <- which(xx >= xlim[1] & xx <= xlim[2])
    yind <- which(yy >= ylim[1] & yy <= ylim[2])
    tind <- which(tt >= tlim[1] & tt <= tlim[2])
    start <- c(min(xind), min(yind), min(tind))
    count <- c(length(xind), length(yind), length(tind))
    res <- list(x = xx[xind], y = yy[yind], time = tt[tind], height = var.get.nc(nc, "height", start, count)/1000)
    close.nc(nc)

    if (length(dim(res$height)) > 2) {
        for (i in 1:dim(res$height)[3]) res$height[,,i] <- res$height[,,i] + t(SOfronts$HOlb[yind, xind][,-dim(SOfronts$HOlb)[2]])/10
    } else {
        res$height <- res$height + t(SOfronts$HOlb[yind, xind][,-dim(SOfronts$HOlb)[2]])/10
    }
    res
}

