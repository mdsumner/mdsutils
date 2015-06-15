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
mm <- map("worldHires", xlim = bbox(tr)[1,], ylim = bbox(tr)[2,])

## Auxiliary environmental data

## Etopo2 used for a mask function - this is very coarse, not good
## enough for this complicated coastline, but works ok for illustration

offs <-  c(443, 2400)
region <-  c(1037, 1352)

topo <- readGDAL("D:\\Azimuth\\DATA\\etopo\\Etopo2.tif",
                p4s = as.character(NA), offset = offs, region.dim = region)
## topo becomes the mask object, captured in a closure by the lookup function
topo <-as.image.SpatialGridDataFrame(topo)
topo$z <- !(topo$z > 0)
lookup <- mkLookup(topo)

## set up the proposal functions
xy <- coordinates(tr)
m <- nrow(xy)

## Choose parameters for proposals appropriately
## m proposals one for each time, 2 params (lon, lat), sigma for each
proposal.x <- norm.proposal(m,  2, c(0.1, 0.1))
## m-1 proposals for each intermediate, 2 params, sigma for each
proposal.z <- norm.proposal(m-1, 2, c(0.2, 0.2))

## interactive session to see if choices are "reasonable" - we want to
## jump around the neighbourhood, but not too much - later we will
## tune the proposals to aid mixing, but the choice now helps the MCMC
## initialize efficiently, especially with respect to the mask The Zs
## need greater proposals as they are generally more varied

plot(xy)
## repeat this line a few times to see what the spread is like
points(proposal.x$proposal(coordinates(xy)))

plot((xy[-m,1:2]+xy[-1,1:2])/2)
## repeat this line a few times to see what the spread is like
points(proposal.x$proposal((xy[-m,1:2]+xy[-1,1:2])/2))


## the choice of behav.mean/sd here is based on building this
## distribution, I'm assuming that the Argos locations are reasonably
## representative (in the absence of more information)

## km/hr because that's what the model expects (unfinished stuff by me ...)
spd <- trackDistance(coordinates(tr), longlat = TRUE) /
diff(unclass(tr$time) / 3600)
x <- seq(0, max(spd) * 1.5, by = 0.1)
op <- par(mfrow = c(2, 1))
plot(x, dnorm(x, mad(spd), sd(spd)), main = "likelihood of speed drops
off fast with higher values")
plot(x, dnorm(x, mad(spd), sd(spd), log = TRUE), main = "log normal as
applied in the model")
par(op)

## choose these values as required, or rewrite the behavioural
function in the model to suit
speed.mu <- mad(spd)
speed.sd <- sd(spd)

tr$class <- ordered(tr$class, levels = c("Z", "B", "A", "0", "1", "2", "3"))
argos.sd <- argos.sigma(cls, sigma = c(100, 80, 50, 20, 10, 8, 4),
adjust = 111.12)

## build the model
## Argos times and locations
## proposal functions for X and Z
## mask functions, and whether release/recapture are fixed (which
helps pin down the start and end)
## starting positions (Zs are intermediate to the Xs)
## Argos position sigma (can pass in a single value, or a value based
on the class)
## behavioural model function (defined for lognormal or gamma)
## mean and sd for behavioural model
d.model <- satellite.model(tr$time, xy, proposal.x$proposal,
proposal.z$proposal,
                          mask.x = lookup, mask.z= lookup,
fix.release = FALSE, fix.recapture = FALSE,
                          start.x = xy, start.z = (xy[-m,1:2]+xy[-1,1:2])/2,
                          posn.sigma = argos.sd, behav = "log",
                          behav.mean = speed.mu, behav.sd = speed.sd)


## Run initial chain to obtain values that meet mask requirements
ch <- metropolis0(d.model,iters=10,thin=10, start.x = d.model$start.x,
start.z = d.model$start.z)
while(!(all(ch$mask.x) && all(ch$mask.z))) {
   ch <- metropolis0(d.model,iters=100,thin=10, start.x = ch$last.x,
start.z = ch$last.z)
   plot(head(ch$last.x), type = "l")
   map(mm, add = TRUE)
}

## run a short period, and check
ch <- metropolis(d.model,iters=200,thin=10,

                   start.x=ch$last.x,
                   start.z=ch$last.z)



plot(tr, pch = 1, col = "grey")
lines(coordinates(tr), col = "grey")
map(mm, add = TRUE)
lines(ch$last.x)
points(ch$last.x, col = "red", pch = 21, cex = 0.5)


## run for a while to settle in
for (i in 1:3) {

   ch <- metropolis(d.model,iters=2000,thin=10,
                   start.x=ch$last.x,
                   start.z=ch$last.z)

}


plot(tr, pch = 1, col = "grey")

map(mm, add = TRUE)
apply(ch$z, 3, points, pch = ".")
lines(coordinates(tr), col = "grey")


## run and tune the proposals (hopefully enough to allow for burn-in)
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
for (i in 1:14) {

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