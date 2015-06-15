speedfilter1 <- function(x, max.speed, pprm = 3, test = FALSE) {
    tor <- x@TOR.columns
    id <- unique(x[[tor[2]]])
    if (length(id) > 1) stop("single-ID trips only")

    ## distfun <- function(x) {
    ##     trackDistance(x, longlat = longlat)
    ## }
    xy <- coordinates(x)
    tms <- x[[tor[1]]]
    npts <- nrow(xy)
    if (pprm%%2 == 0 || pprm < 3) {
        stop("Points per running mean should be odd and greater than 3, pprm = 3")
    }
    ##  FUN <- function(x, aadBUG = FALSE) {
    ##     sqrt(sum((x)^2, na.rm = FALSE)/(if (aadBUG) 1 else length(x)))
    ## }
    RMS <- rep(max.speed + 1, npts)
    offset <- pprm - 1
    ok <- rep(TRUE, npts)
    if (npts < (pprm + 1)) {
        warning("Not enough points to filter ID: \"", id,  "\"\n continuing . . . \n")
        return(ok)
    }
    index <- 1:npts
    iter <- 1
    while (any(RMS > max.speed, na.rm = TRUE)) {
        n <- length(which(ok))
        speed1 <- trackDistance(xy[ok, ], longlat = longlat)/(diff(unclass(tms[ok]))/3600)
        speed2 <- trackDistance(xy[ok, ], longlat = longlat, push = 2)/((unclass(tms[ok][-c(1, 2)]) - unclass(tms[ok][-c(n - 1, n)]))/3600)

        ## maybe generalize to 1, 2, 3, etc. speed levels?
        ## nlevs <- 2
        ## for (si in 2:nlevs) {
        ##     s <- trackDistance(xy[ok, ], longlat = longlat, push = si)/((unclass(tms[ok][-(1:si)]) - unclass(tms[ok][-((n - si + 1):n)]))/3600)
        ## }

        thisIndex <- index[ok]
        npts <- length(speed1)
        if (npts < pprm) {
            next
        }

        sub1 <- rep(1:2, npts - offset) + rep(1:(npts - offset), each = 2)
        sub2 <- rep(c(0, 2), npts - offset) + rep(1:(npts - offset), each = 2)
        rmsRows <- cbind(matrix(speed1[sub1], ncol = offset, byrow = TRUE), matrix(speed2[sub2], ncol = offset, byrow = TRUE))

##        RMS <- c(rep(0, offset), apply(rmsRows, 1, FUN, aadBUG = aadBUG))
        ##  FASTER
        RMS <- c(rep(0, offset), sqrt(rowSums(rmsRows^2)/ncol(rmsRows)))

        if (test & iter == 1) {
            res$speed <- c(res$speed, 0, speed1)
            res$rms <- c(res$rms, 0, RMS)
            break
        }
        iter <- iter + 1
        bad <- RMS > max.speed
        bad[is.na(bad)] <- FALSE
        segs <- cumsum(c(0, abs(diff(bad))))
        segs[RMS <= max.speed] <- NA
        peaks <- tapply(RMS, segs, which.max)
        for (i in levels(as.factor(segs))) {
            RMS[segs == i & !is.na(segs)][peaks[[i]]] <- NA
        }
        RMS[1] <- 0
        RMS[length(RMS)] <- 0
        ok[thisIndex][is.na(RMS)] <- FALSE
    }
}






load("E:\\mike\\working\\doc\\PhD\\Thesis-MDSUMNER\\figuredata\\RawArgos.Rdata")
tr <- tr[tr$seal == "c026", ]
