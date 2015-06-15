BASsegments <-function(light) {
        nl <- length(light)
        flag <- rep(NA, nl)
        stillzero <- NULL
        newseg <- 0
        for (i in 1:nl) {
                if (light[i] == 0) {
                        stillzero <- c(stillzero, i)
                        newseg <- newseg + 1
                } else {
                        stillzero <- NULL
                }
                if(length(stillzero) > 25) {
                        flag[stillzero] <- newseg
                }
                if (i %% 1000 == 0) cat(i, " of ", nl, "\n")
        }
        ## process the flag
        pk <- NULL
        uflags <- unique(flag[!is.na(flag)])

        for (i in uflags ) {
                ids <- range(which(flag == i))
                imin <- min(ids)
                lastlight <- light[imin]
                foundit <- FALSE
                while(!foundit) {
                        lastlight <- light[imin]
                        imin <- imin - 1
                        if (lastlight == 64) foundit <- TRUE
                }
                imax <- max(ids)
                lastlight <- light[imax]
                foundit <- FALSE
                while(!foundit & imax <= nl) {
                        lastlight <- light[imax]
                        imax <- imax + 1
                        if (lastlight == 64) foundit <- TRUE
                }
                pk <- c(pk, imin - 4, min(ids) + 4,
                            max(ids) - 4, imax+ 4)
        }

        pk <- pk[!pk > nl]
        pk <- c(pk, nl)
        segment <- picksegs(pk, nl)
        segment
}
