### Calibration files

data <- read.table(tabfile, header = T)
data$gmt <- as.POSIXct(strptime(as.character(data$date),
                             "%d-%m-%Y %H:%M:%S"),"GMT") 
data$gmt[1]

elevation.gmt <- mkElevation(data$gmt)
data$elev <- elevation.gmt(Tlat,Tlon)

## get rid of stuff when in box (and enough to be outside min/max range
data <- data[data$gmt > dataStart & data$gmt < calibEnd,]

## Check elevations look ok
plot(light ~ elev,data=data,pch=".")
abline(v=c(-5,3))


## for construction of the segments you don't care too much about the
## elevation range, trim it down later.
elev.min <- -10
elev.max <- 10
keep <- (data$elev > elev.min & data$elev < elev.max)
segs <- cumsum(c(0,abs(diff(keep))))
data$segs <- segs

## Was Mark out with his torch saturday night?
#st <- 1
#en <- 10000
#plot(data$gmt[st:en],data$light[st:en],pch=".")
#lines(data$gmt[st:en],data$elev[st:en]+150,col = "blue")
#lines(data$gmt,segs+140)
#abline(v=data$gmt[which(abs(diff(keep))!=0)])


## Check out elevations only for the data we will keep
plot(light ~ elev,data=data[keep,],pch=".")
abline(v=c(-5,3))


## First trim to the segments we want
data <- data[keep,]

d.calib <- data.frame(light = data$light,segment = data$segs, elevation = data$elev)

write.table(d.calib,calfile)
rm(data,d.calib)
#####################################################
#####################################################
