metropNoZ <- function (model, iters = 1000, thin = 10, start.x = NULL, start.z = NULL, Z = TRUE)
{
    x0 <- start.x
    z0 <- start.z
    if (is.null(x0))
        x0 <- model$start.x
    if (is.null(z0))
        z0 <- model$start.z
    dimnames(x0) <- NULL
    dimnames(z0) <- NULL
    proposal.x <- model$proposal.x
    proposal.z <- model$proposal.z
    mask.x <- model$mask.x
    mask.z <- model$mask.z
    logp.position <- model$logp.position
    logp.behavioural <- model$logp.behavioural
    fixed.x <- model$fixed.x
    n <- nrow(x0)
    ch.x <- array(0, c(dim(x0), iters))
    ch.z <- array(0, c(n - 1, 2, iters))
    logp.posn0 <- logp.position(x0)
    print(paste("k1 of ", iters))
    zfac <- Z * 1
    for (k1 in 1:iters) {
        if (k1%%10 == 0)
            print(k1)
        for (k2 in 1:thin) {
            x1 <- proposal.x(x0)
            x1[fixed.x, ] <- x0[fixed.x, ]
            mask.x1 <- mask.x(x1)
            logp.posn1 <- logp.position(x1)
            if (mask.x1[1]) {
                logp0 <- logp.posn0[1] + zfac * (logp.behavioural(1,
                  x0[1, 1:2, drop = FALSE], z0[1, , drop = FALSE],
                  x0[2, 1:2, drop = FALSE]))
                logp1 <- logp.posn1[1] + zfac * (logp.behavioural(1,
                  x1[1, 1:2, drop = FALSE], z0[1, , drop = FALSE],
                  x1[2, 1:2, drop = FALSE]))
                if (logp1 - logp0 > log(runif(1))) {
                  x0[1, ] <- x1[1, ]
                  logp.posn0[1] <- logp.posn1[1]
                }
            }
            if (mask.x1[n]) {
                logp0 <- logp.posn0[n] + zfac * (logp.behavioural(n -
                  1, x0[n - 1, 1:2, drop = FALSE], z0[n - 1,
                  , drop = FALSE], x0[n, 1:2, drop = FALSE]))
                logp1 <- logp.posn1[n] + zfac * (logp.behavioural(n -
                  1, x1[n - 1, 1:2, drop = FALSE], z0[n - 1,
                  , drop = FALSE], x1[n, 1:2, drop = FALSE]))
                if (logp1 - logp0 > log(runif(1))) {
                  x0[n, ] <- x1[n, ]
                  logp.posn0[n] <- logp.posn1[n]
                }
            }
            for (rb in 2:3) {
                is <- seq(rb, n - 1, by = 2)
                is <- is[mask.x1[is]]
                logp0 <- (logp.posn0[is] + zfac * (logp.behavioural(is -
                  1, x0[is - 1, 1:2, drop = FALSE], z0[is - 1,
                  , drop = FALSE], x0[is, 1:2, drop = FALSE]) +
                  logp.behavioural(is, x0[is, 1:2, drop = FALSE],
                    z0[is, , drop = FALSE], x0[is + 1, 1:2, drop = FALSE])))
                logp1 <- (logp.posn1[is] + zfac * (logp.behavioural(is -
                  1, x1[is - 1, 1:2, drop = FALSE], z0[is - 1,
                  , drop = FALSE], x1[is, 1:2, drop = FALSE]) +
                  logp.behavioural(is, x1[is, 1:2, drop = FALSE],
                    z0[is, , drop = FALSE], x1[is + 1, 1:2, drop = FALSE])))
                accept <- is[logp1 - logp0 > log(runif(length(is)))]
                x0[accept, ] <- x1[accept, ]
                logp.posn0[accept] <- logp.posn1[accept]
            }
            z1 <- proposal.z(z0)
            is <- (1:(n - 1))[mask.z(z1)]
            logp0 <- zfac * (logp.behavioural(is, x0[is, 1:2, drop = FALSE],
                z0[is, ], x0[is + 1, 1:2, drop = FALSE]))
            logp1 <- zfac * (logp.behavioural(is, x0[is, 1:2, drop = FALSE],
                z1[is, ], x0[is + 1, 1:2, drop = FALSE]))
            accept <- is[(logp1 - logp0 > log(runif(length(is))))]
            z0[accept, ] <- z1[accept, ]
        }
        ch.x[, , k1] <- x0
        ch.z[, , k1] <- z0
    }
    list(model = model, x = ch.x, z = ch.z, last.x = x0, last.z = z0)
}


ch <- metropNoZ(d.model,iters=100,thin=10, start.x=ch$last.x, start.z = ch$last.z, Z = FALSE)
