ff.bin <- function(Array, pimgs, weights = NULL, chunk = 2000, proj = NULL)
{
    dm <- dim(Array)
    cnt <- round(seq(0, 100, length = dm[3]/chunk))
    i <- 1
    cat("\n")
    n <- dm[1] * dm[2] * as.integer(chunk)
    start.n <- 1
    repeat {
        A <- Array[,,start.n:max(dm[3], (start.n + chunk-1))]
        if (!is.null(proj))
            A[, 1:2, ] <- apply(A[, 1:2, ], 3, function(x) project(x,
                proj))
        pimgs <- behav.bin(A, pimgs, weights = weights)
        cat(cnt[i], "...\n", sep = "")
        i <- i + 1

        if (start.n > dm[3] - chunk) {
            break
        }
        start.n <- start.n + chunk
    }
    cat("\n")
    pimgs
}

 dims <- c(141, 2, 100000)


  v.data <-   ff(vmode="double", dim = dims, dimnames = list(NULL, NULL, NULL),
                   filename = "tempA.ff3",
                   overwrite = TRUE, finalizer = "close", caching="mmeachflush")

con <- file("X1.bin", open = "rb")
 dm <- readBin(con, "integer", 3)

chunk <- 2000

n <- prod(c(dims[-3], chunk))
start.n <- 1
for (i in 1:(dims[3]/chunk)) {

	v.data[,,start.n:(i * chunk)] <- array(readBin(con, "double", n), c(dims[-3], chunk))
        start.n <- start.n + chunk

}

close(con)


bin large array in metropolis

allow small runs at the beginning
- by user-specified iters=1000 etc.
- object keeps track of iters.already.run
- user can always restart
- final run simply fleshes out the remaining iterations
- but what about cancelling . . .

 - can the ch and proposal objects exist as standalones, with <<- ??

 - then metropolis would run as a no-return function
 - ch could also store a history, comprising the populated array
  - on exit? etc.




op <- par(mfrow = c(2, 1))
plot(x, dnorm(x, mad(spd), sd(spd)), main = "likelihood of speed drops off fast with higher values")
plot(x, dnorm(x, mad(spd), sd(spd), log = TRUE), main = "log normal as applied in the model")
par(op)
## choose these values as required, or rewrite the behavioural function in the model to suit
speed.mu <- mad(spd)
speed.sd <- sd(spd)
## build the model
## Argos times and locations
## proposal functions for X and Z
## mask functions, and whether release/recapture are fixed (which helps pin down the start and end)
## starting positions (Zs are intermediate to the Xs)
## Argos position sigma (can pass in a single value, or a value based on the class)
## behavioural model function (defined for lognormal or gamma)
## mean and sd for behavioural model
d.model <- satellite.model(tr$time, coordinates(tr), proposal.x$proposal, proposal.z$proposal,
mask.x = lookup, mask.z= lookup, fix.release = FALSE, fix.recapture = FALSE,
start.x = xy, start.z = (xy[-m,1:2]+xy[-1,1:2])/2,
     posn.sigma = 2, behav = "log",
    behav.mean = speed.mu, behav.sd = speed.sd)
## Run initial chain to obtain values that meet mask requirements
ch <- metropolis0(d.model,iters=10,thin=10, start.x = d.model$start.x, start.z = d.model$start.z)
while(!(all(ch$mask.x) && all(ch$mask.z))) {
    ch <- metropolis0(d.model,iters=100,thin=10, start.x = ch$last.x, start.z = ch$last.z)
    plot(head(ch$last.x), type = "l")
    map(mm, add = TRUE)
}
ch <- metropolis(d.model,iters=200,thin=10,
                    start.x=ch$last.x,
                    start.z=ch$last.z)
plot(tr, pch = 1, col = "grey")
lines(coordinates(tr), col = "grey")
map(add = T)
lines(ch$last.x)
points(ch$last.x, col = "red", pch = 21, cex = 0.5)
for (i in 1:3) {
    ch <- metropolis(d.model,iters=2000,thin=10,
                    start.x=ch$last.x,
                    start.z=ch$last.z)
    plot(tr, pch = 1, col = "grey")
    lines(coordinates(tr), col = "grey")
    map(add = T)
    lines(ch$last.x)
    points(ch$last.x, col = "red", pch = 21, cex = 0.5)
}
?argosClass
library(help = tripEstimation)
?satellite.model
library(help = trip)
?argos.sigma
ringy$class
argos.sd <- argos.sigma(ringy$class, sigma = c(100, 80, 50, 20, 10, 4, 2), adjust = 111.12)
argos.sd
table(argos.sd)
levels(argos.sd)
argos.sd
tapply(argos.sd, unique)
tapply(argos.sd, names(argos.sd), unique)
ringy$class
?argos.sigma
cls <- ordered(ringy$class, levels = c("Z", "B", "A", "0", "1", "2", "3"))
argos.sd <- argos.sigma(cls, sigma = c(100, 80, 50, 20, 10, 4, 2), adjust = 111.12)
 tapply(argos.sd, names(argos.sd), unique)
cls <- ordered(ringy$class, levels = c("Z", "B", "A", "0", "1", "2", "3"))
argos.sd <- argos.sigma(cls, sigma = c(100, 80, 50, 20, 10, 4, 2), adjust = 111.12)
## build the model
## Argos times and locations
## proposal functions for X and Z
## mask functions, and whether release/recapture are fixed (which helps pin down the start and end)
## starting positions (Zs are intermediate to the Xs)
## Argos position sigma (can pass in a single value, or a value based on the class)
## behavioural model function (defined for lognormal or gamma)
## mean and sd for behavioural model
d.model <- satellite.model(tr$time, coordinates(tr), proposal.x$proposal, proposal.z$proposal,
mask.x = lookup, mask.z= lookup, fix.release = FALSE, fix.recapture = FALSE,
start.x = xy, start.z = (xy[-m,1:2]+xy[-1,1:2])/2,
     posn.sigma = argos.sd, behav = "log",
    behav.mean = speed.mu, behav.sd = speed.sd)
## Run initial chain to obtain values that meet mask requirements
ch <- metropolis0(d.model,iters=10,thin=10, start.x = d.model$start.x, start.z = d.model$start.z)
while(!(all(ch$mask.x) && all(ch$mask.z))) {
    ch <- metropolis0(d.model,iters=100,thin=10, start.x = ch$last.x, start.z = ch$last.z)
    plot(head(ch$last.x), type = "l")
    map(mm, add = TRUE)
}
cls <- ordered(ringy$class, levels = c("Z", "B", "A", "0", "1", "2", "3"))
argos.sd <- argos.sigma(cls, sigma = c(200, 180, 150, 20, 10, 4, 2), adjust = 111.12)
proposal.x <- norm.proposal(m,  2, cbind(argos.sd, argos.sd))
plot(tr)
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
axis(1)
axis(2)
cls <- ordered(ringy$class, levels = c("Z", "B", "A", "0", "1", "2", "3"))
argos.sd <- argos.sigma(cls, sigma = c(100, 80, 50, 20, 10, 4, 2), adjust = 111.12)
 proposal.x <- norm.proposal(m,  2, cbind(argos.sd, argos.sd))
plot(head(tr))
plot(head(coordinates(tr)))
plot(head(coordinates(tr), 30))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
points(proposal.x$proposal(coordinates(tr)))
plot(tr[tr$class > 1, ])
names(tr)
tr$class
plot(tr[tr$class > "1", ])
tr$class <- ordered(tr$class, levels = c("Z", "B", "A", "0", "1", "2", "3"))
plot(tr[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr)[tr$class > 1, ]))
points(proposal.x$proposal(coordinates(tr)[tr$class > 1, ]))
points(proposal.x$proposal(coordinates(tr)[tr$class > 1, ]))
plot(tr[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
points(proposal.x$proposal(coordinates(tr))[tr$class > 1, ])
## Choose parameters for proposals appropriately
## m proposals one for each time, 2 params (lon, lat), sigma for each
proposal.x <- norm.proposal(m,  2, c(0.1, 0.1))
## m-1 proposals for each intermediate, 2 params, sigma for each
proposal.z <- norm.proposal(m-1, 2, c(0.2, 0.2))
tr$class <- ordered(tr$class, levels = c("Z", "B", "A", "0", "1", "2", "3"))
argos.sd <- argos.sigma(cls, sigma = c(100, 80, 50, 20, 10, 8, 4), adjust = 111.12)
## build the model
## Argos times and locations
## proposal functions for X and Z
## mask functions, and whether release/recapture are fixed (which helps pin down the start and end)
## starting positions (Zs are intermediate to the Xs)
## Argos position sigma (can pass in a single value, or a value based on the class)
## behavioural model function (defined for lognormal or gamma)
## mean and sd for behavioural model
d.model <- satellite.model(tr$time, coordinates(tr), proposal.x$proposal, proposal.z$proposal,
mask.x = lookup, mask.z= lookup, fix.release = FALSE, fix.recapture = FALSE,
start.x = xy, start.z = (xy[-m,1:2]+xy[-1,1:2])/2,
     posn.sigma = argos.sd, behav = "log",
    behav.mean = speed.mu, behav.sd = speed.sd)
## Run initial chain to obtain values that meet mask requirements
ch <- metropolis0(d.model,iters=10,thin=10, start.x = d.model$start.x, start.z = d.model$start.z)
while(!(all(ch$mask.x) && all(ch$mask.z))) {
    ch <- metropolis0(d.model,iters=100,thin=10, start.x = ch$last.x, start.z = ch$last.z)
    plot(head(ch$last.x), type = "l")
    map(mm, add = TRUE)
}
ch <- metropolis(d.model,iters=200,thin=10,
                    start.x=ch$last.x,
                    start.z=ch$last.z)
plot(tr, pch = 1, col = "grey")
lines(coordinates(tr), col = "grey")
map(add = T)
lines(ch$last.x)
points(ch$last.x, col = "red", pch = 21, cex = 0.5)
for (i in 1:3) {
    ch <- metropolis(d.model,iters=2000,thin=10,
                    start.x=ch$last.x,
                    start.z=ch$last.z)
    plot(tr, pch = 1, col = "grey")
    lines(coordinates(tr), col = "grey")
    map(add = T)
    lines(ch$last.x)
    points(ch$last.x, col = "red", pch = 21, cex = 0.5)
}
apply(ch$last.z, 3, points, pch = ".")
names(ch)
apply(ch$z, 3, points, pch = ".")
args(chain.write)
## run and tune the proposals
for (i in 1:4) {
  ch <- metropolis(d.model,iters=2000,thin=10,
                    start.x=ch$last.x,
                    start.z=ch$last.z)
proposal.x$tune(ch$x, scale = 0.3)
proposal.z$tune(ch$z, scale = 0.3)
}
## run and save the results to disk, tuning as we go
xfile <- "X0.bin"
zfile <- "Z0.bin"
for (i in 1:4) {
  ch <- metropolis(d.model,iters=2000,thin=10,
                    start.x=ch$last.x,
                    start.z=ch$last.z)
  proposal.x$tune(ch$x, scale = 0.3)
  proposal.z$tune(ch$z, scale = 0.3)
  chain.write(xfile, ch$x, append = !i == 1)
  chain.write(xfile, ch$x, append = !i == 1)
}
i
i <- 1
 chain.write(xfile, ch$x, append = !i == 1)
    chain.write(zfile, ch$x, append = !i == 1)
for (i in 2:14) {
    ch <- metropolis(d.model,iters=2000,thin=10,
                     start.x=ch$last.x,
                     start.z=ch$last.z)
    proposal.x$tune(ch$x, scale = 0.3)
    proposal.z$tune(ch$z, scale = 0.3)
    chain.write(xfile, ch$x, append = !i == 1)
    chain.write(zfile, ch$x, append = !i == 1)
}
i <- 1
 chain.write(xfile, ch$x, append = !i == 1)
    chain.write(zfile, ch$z, append = !i == 1)
for (i in 2:14) {
    ch <- metropolis(d.model,iters=2000,thin=10,
                     start.x=ch$last.x,
                     start.z=ch$last.z)
    proposal.x$tune(ch$x, scale = 0.3)
    proposal.z$tune(ch$z, scale = 0.3)
    chain.write(xfile, ch$x, append = !i == 1)
    chain.write(zfile, ch$z, append = !i == 1)
}
xfile <- "X1.bin"
zfile <- "Z1.bin"
for (i in 1:64) {
    ch <- metropolis(d.model,iters=2000,thin=10,
                     start.x=ch$last.x,
                     start.z=ch$last.z)
    proposal.x$tune(ch$x, scale = 0.3)
    proposal.z$tune(ch$z, scale = 0.3)
    chain.write(xfile, ch$x, append = !i == 1)
    chain.write(zfile, ch$z, append = !i == 1)
}
xfile <- "X2.bin"
zfile <- "Z2.bin"
for (i in 1:64) {
    ch <- metropolis(d.model,iters=2000,thin=10,
                     start.x=ch$last.x,
                     start.z=ch$last.z)
    proposal.x$tune(ch$x, scale = 0.3)
    proposal.z$tune(ch$z, scale = 0.3)
    chain.write(xfile, ch$x, append = !i == 1)
    chain.write(zfile, ch$z, append = !i == 1)
}
xfile <- "X3.bin"
zfile <- "Z3.bin"
for (i in 1:64) {
    ch <- metropolis(d.model,iters=2000,thin=10,
                     start.x=ch$last.x,
                     start.z=ch$last.z)
    proposal.x$tune(ch$x, scale = 0.3)
    proposal.z$tune(ch$z, scale = 0.3)
    chain.write(xfile, ch$x, append = !i == 1)
    chain.write(zfile, ch$z, append = !i == 1)
}
i
flipHorizontal <- function(x) {
       if (!inherits(x, "SpatialGridDataFrame")) stop("x must be a
SpatialGridDataFrame")
       grd <- getGridTopology(x)
       idx = 1:prod(grd@cells.dim[1:2])
       m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow =
TRUE)[,grd@cells.dim[1]:1]
       idx = as.vector(t(m))
       x@data <- x@data[idx, TRUE, drop = FALSE]
       x
}
flipVertical <- function(x) {
       if (!inherits(x, "SpatialGridDataFrame")) stop("x must be a
SpatialGridDataFrame")
       grd <- getGridTopology(x)
       idx = 1:prod(grd@cells.dim[1:2])
       m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow =
TRUE)[grd@cells.dim[2]:1, ]
       idx = as.vector(t(m))
       x@data <- x@data[idx, TRUE, drop = FALSE]
       x
}
image(topo)
flipVertical(10)
contour(flipVertical(topo), add = T)
contour(flipVertical(image2Grid(topo)), add = T)
ls()
names(l)
class(l)
length(l)
names(l[[1]])
mm
ls9)
ls()
library(ff)
#    repeat {
 #       A <- readBin(con, "double", n)
  #      m <- as.integer(length(A)/(dm[1] * dm[2]))
   #     if (m == 0)
    #        break
     #   A <- array(A, c(dm[-3], m))
      #  if (!is.null(proj))
       #     A[, 1:2, ] <- apply(A[, 1:2, ], 3, function(x) project(x,
        #        proj))
       # pimgs <- behav.bin(A, pimgs, weights = weights)
      #  cat(cnt[i], "...\n", sep = "")
     #   i <- i + 1
   # }
library(tripEstimation)
library(diveMove)
library(trip)
library(mapdata)
library(rgdal)
## prepare source data
locs <- readLocs(system.file(file.path("data", "sealLocs.csv"),
                        package="diveMove"), idCol=1, dateCol=2,
                        dtformat="%Y-%m-%d %H:%M:%S", classCol=3,
                        lonCol=4, latCol=5)
ringy <- subset(locs, id == "ringy" & !is.na(lon) & !is.na(lat))
coordinates(ringy) <- ~lon+lat
tr <- trip(ringy, c("time", "id"))
## set up the proposal functions
xy <- coordinates(tr)
m <- nrow(xy)
m
?ff
metropolis
  dims <- c(141, 2, 10000)
  v.data <-   ff(vmode="double", dim = dims, dimnames = list(NULL, NULL, NULL, NULL),
                   filename = "tempA.ff",
                   overwrite = overwrite, finalizer = "close", caching="mmeachflush")
  v.data <-   ff(vmode="double", dim = dims, dimnames = list(NULL, NULL, NULL, NULL),
                   filename = "tempA.ff",
                   overwrite = TRUE, finalizer = "close", caching="mmeachflush")
getwd()
setwd("C:/temp")
  v.data <-   ff(vmode="double", dim = dims, dimnames = list(NULL, NULL, NULL, NULL),
                   filename = "tempA.ff",
                   overwrite = TRUE, finalizer = "close", caching="mmeachflush")
  v.data <-   ff(vmode="double", dim = dims, dimnames = list(NULL, NULL, NULL),
                   filename = "tempA.ff",
                   overwrite = TRUE, finalizer = "close", caching="mmeachflush")
  v.data <-   ff(vmode="double", dim = dims, dimnames = list(NULL, NULL, NULL),
                   filename = "tempA.ff1",
                   overwrite = TRUE, finalizer = "close", caching="mmeachflush")
chunk.bin
con <- file("X0.bin", open = "rb")
n <- prod(c(dims[-3], 2000))
start.n <- 1
for (i in 1:5) {
v.data[,,start.n:((i-1) * 2000)] <- array(readBin(con, "double", n), c(dims[-3], 2000))
}
close(con)
con <- file("X0.bin", open = "rb")
n <- prod(c(dims[-3], 2000))
start.n <- 1
for (i in 1:5) {
v.data[,,start.n:(i * 2000)] <- array(readBin(con, "double", n), c(dims[-3], 2000))
        start.n <- start.n + 2000
}
close(con)
dim(v.data)
plot(v.data[,,1])
plot(v.data[,,10000])
c(dims[-3], 2000)
con <- file("X1.bin", open = "rb")
n <- prod(c(dims[-3], 2000))
start.n <- 1
for (i in 1:5) {
v.data[,,start.n:(i * 2000)] <- array(readBin(con, "double", n), c(dims[-3], 2000))
        start.n <- start.n + 2000
}
close(con)
plot(v.data[,,1])
chunk.bin
con <- file("X1.bin", open = "rb")
 dm <- readBin(con, "integer", 3)
n <- prod(c(dims[-3], 2000))
start.n <- 1
for (i in 1:5) {
v.data[,,start.n:(i * 2000)] <- array(readBin(con, "double", n), c(dims[-3], 2000))
        start.n <- start.n + 2000
}
close(con)
plot(v.data[,,1])
plot(v.data[,,10000])
prod(dim(v.data))
prod(dim(v.data)) * 8
prod(dim(v.data)) * 8 / 1e6
  dims <- c(141, 2, 100000)
  v.data <-   ff(vmode="double", dim = dims, dimnames = list(NULL, NULL, NULL),
                   filename = "tempA.ff2",
                   overwrite = TRUE, finalizer = "close", caching="mmeachflush")
con <- file("X1.bin", open = "rb")
 dm <- readBin(con, "integer", 3)
n <- prod(c(dims[-3], 2000))
start.n <- 1
for (i in 1:5) {
v.data[,,start.n:(i * 2000)] <- array(readBin(con, "double", n), c(dims[-3], 2000))
        start.n <- start.n + 2000
}
close(con)
  dims <- c(141, 2, 100000)
  v.data <-   ff(vmode="double", dim = dims, dimnames = list(NULL, NULL, NULL),
                   filename = "tempA.ff2",
                   overwrite = TRUE, finalizer = "close", caching="mmeachflush")
con <- file("X1.bin", open = "rb")
 dm <- readBin(con, "integer", 3)
chunk <- 2000
n <- prod(c(dims[-3], chunk))
start.n <- 1
for (i in 1:(dims[3]/chunk)) {
v.data[,,start.n:(i * 2000)] <- array(readBin(con, "double", n), c(dims[-3], 2000))
        start.n <- start.n + 2000
}
close(con)
rm(v.data)
  dims <- c(141, 2, 100000)
  v.data <-   ff(vmode="double", dim = dims, dimnames = list(NULL, NULL, NULL),
                   filename = "tempA.ff3",
                   overwrite = TRUE, finalizer = "close", caching="mmeachflush")
con <- file("X1.bin", open = "rb")
 dm <- readBin(con, "integer", 3)
chunk <- 2000
n <- prod(c(dims[-3], chunk))
start.n <- 1
for (i in 1:(dims[3]/chunk)) {
v.data[,,start.n:(i * 2000)] <- array(readBin(con, "double", n), c(dims[-3], 2000))
        start.n <- start.n + 2000
}
close(con)
plot(v.data[,,1])
plot(v.data[,,dims[3]])
ff.bin <- function(Array, pimgs, weights = NULL, chunk = 2000, proj = NULL)
{
    dm <- dim(Array)
    cnt <- round(seq(0, 100, length = dm[3]/chunk))
    i <- 1
    cat("\n")
    n <- dm[1] * dm[2] * as.integer(chunk)
    start.n <- 1
    repeat {
        A <- Array[,,start.n:max(dm[3], (start.n + chunk-1))]
        if (!is.null(proj))
            A[, 1:2, ] <- apply(A[, 1:2, ], 3, function(x) project(x,
                proj))
        pimgs <- behav.bin(A, pimgs, weights = weights)
        cat(cnt[i], "...\n", sep = "")
        i <- i + 1
        if (start.n > dm[3] - chunk) {
            break
        }
        start.n <- start.n + chunk
    }
    cat("\n")
    pimgs
}
bb <- bbox(tr) + matrix(c(-1, -1, 1, 1), ncol = 2)
Z <- pimg.list(tr$time, xlim = bb[1,], ylim = bb[2,], c(300, 300), Z = TRUE)
Z <- ff.bin(v.data, Z)
dim(v.data)
length(Z)
Z <- ff.bin(v.data[-1,,], Z)
image(combine(Z))
library(trip)
image(combine(Z), col = oc.colors(256))
for (i in 5:m) image(combine(Z, (i-4):(i+4)), col = oc.colors(256))
for (i in seq(5, m, by = 6)) image(combine(Z, (i-4):(i+4)), col = oc.colors(256))
?on.exit
chain
  require(graphics)
     opar <- par(mai = c(1,1,1,1))
     on.exit(par(opar))
plot(1)
history(Inf)

