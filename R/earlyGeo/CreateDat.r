
## structure of .dat files

##  date,time,segments,light

data <- read.table(tabfile, header = T)
data$gmt <- as.POSIXct(strptime(as.character(data$date),
                             "%d-%m-%Y %H:%M:%S"),"GMT") 

elevation.gmt <- mkElevation(data$gmt)
data$elev <- elevation.gmt(Tlat,Tlon)

data <- data[data$gmt > calibEnd & data$gmt < dataEnd,]

## make segments

elev.max <- 10
elev.min <- -10
keep <- (data$elev > elev.min & data$elev < elev.max)

segs <- cumsum(c(0,abs(diff(keep))))
nsegs <- length(unique(segs))
data.dat <- data.frame(light=data$light[keep],
			elevation=data$elev[keep],
			segment=factor(segs[keep]),
			date=data$date[keep])

write.table(data.dat, datfile,row.names = F)
rm(data.dat,data,segs)


