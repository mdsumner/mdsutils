

library(tripEstimation)

d <- d[d$depth < 20, ]


ind <- 1:1000

times <- d$gmt[ind]
light <- d$light[ind]


sun <- solar(times)
elev <- elevation(start[1], start[2], sun)


times0 <- seq(min(times), max(times), length = length(ind))

sun0 <- solar(times0)
SOLALT <- elevation(start[1], start[2], sun0)


Kb <- rep(NA,length(SOLALT))

## estimate light for all solar altitudes below 50 but above 0

Kb[SOLALT >= 0 & SOLALT <= 50] <- 62.134-0.75885*SOLALT[SOLALT >= 0 & SOLALT <= 50] + 0.27749*SOLALT[SOLALT >= 0 & SOLALT <= 50]^2 - 0.012108*SOLALT[SOLALT >= 0 & SOLALT <= 50]^3 + 0.0002052*SOLALT[SOLALT >= 0 & SOLALT <= 50]^4 - 1.2278*10^-6*SOLALT[SOLALT >= 0 & SOLALT <= 50]^5

## estimate for altitudes between 50 and 60 degrees

Kb[SOLALT > 50 & SOLALT <= 60] <- 103 + 0.2*(SOLALT[SOLALT > 50 & SOLALT <= 60]-50)

## altitudes greater than 60

Kb[SOLALT > 60] <- 105


plot(times0, SOLALT,ylim=c(-40,110))
plot(times0,Kb - 50,col="red")
lines(times, light*.2 + 10)


