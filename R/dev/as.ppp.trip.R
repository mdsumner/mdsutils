as.ppp.trip <- function (X, marks = NULL)
{
    require(spatstat)
    bb <- bbox(X)
    colnames(bb) <- NULL
    W <- owin(bb[1, ], bb[2, ])
    nc <- ncol(X)

    torlevs <- X[[X@TOR.columns[2]]]

    ntrips <- length(unique(tor))
    cc2 <- cc1 <- matrix(as.numeric(NA), nrow(X) - ntrips, ncol(X))

    starts <- match(unique(torlevs), torlevs)
    for (i in 1:ntrips) {
        this <- which(tor == unique(tor)[i])
        this.start <- starts[i] - i + 1
        this.ind <- this.start:(this.start + length(this) - 1)
        cc1[-length(this.ind) - c(0, 1) + 1, ] <- coordinates(X)[this[-(length(this) - c(0, 1))], ]
        cc2[this.ind[-1], ] <- coordinates(X)[this.ind[-1], ]

    }


    marks <- if (nc == 0)
        NULL
    else X[[X@TOR.names[2]]]
    if (nc > 0)
        warning(paste(nc - 1, "columns of data frame discarded"))
    cc <- coordinates(X)
    return(ppp(cc[, 1], cc[, 2], window = W, marks = marks, check = FALSE))
}
