### This taken from the working toolshed method in:
### G:\mdsumner\CODE\Rfunctions\Restimation
###
### This is a new version, replacing that in
### AZIMUTH//:\Documents and Settings\mdsumner\My Documents\
###    PROJECTS\MCMCGeo\MarkGeolocation
###  - which has been backed up on Antarctique//G:/BKUP/azimuth_16Apr2004
### on the 15 April 2004.  sMat.r has been reworked a lot.

## source("elev.r")
## source("calib.r")

srcDir <- "C:/Documents and Settings/mdsumner/My Documents/CODE/R/src/"
source(paste(srcDir,"sMat.r",sep=""))
source(paste(srcDir,"funcLib.r",sep=""))

####
## run stuff in specifics.r
###

data <- read.table(rawfile,sep = ",", header = T)

data$gmt <- as.POSIXct(strptime(paste(data$date,data$time),
                            "%d-%m-%Y %H:%M:%S"),"GMT") 



elevation.gmt <- mkElevation(data$gmt)
data$elev <- elevation.gmt(Tlat,Tlon)


#st <- 1
#en <- st + 70000
#plot(data$gmt[st:en],data$light[st:en])
#lines(data$gmt,2*(data$elev+60),col = "red")

#data$gmt[1]
data$gmt <- NULL
## tag for these data:

write.table(data,paste(label,".tab",sep = ""))
#
#################################################################


#########################
#########################


#############################################################################
#############################################################################
##  
## Create data files


## algorithm to calculate solar elevations at given time/location


tabfile <- paste(label,".tab",sep = "")

###################
## Calibration data 
## A number of plots are produced to look at the data used
## for calibration, this is an area for much improvement

####
####  Calibration
calfile <- paste(label,".cal",sep = "")
source(paste(srcDir,"CreateCal.r",sep=""))

#############################################################################
####
####  Calibration

## An xyplot is produced of light ~ elevation by segment - use to
## select best segments for calibration

library(mgcv)

data.calib <- read.table(calfile,header=T)
library(lattice)
xyplot(light ~ elevation | segment,type="l",data=data.calib)

fit <- gam(light ~ s(elevation,k=20) + segment,
           family=gaussian(link="identity"),data=data.calib)

elev.max <- 7
elev.min <- -10
elev.cal <- seq(elev.min,elev.max,length=500)

new.data <- data.frame(elevation = elev.cal,
                       segment = rep(data.calib$segment[1],
                         length(elev.cal)))


light.cal <- predict(fit,new.data,type="response")
calib <- approxfun(elev.cal,light.cal,rule=2)
rm(data.calib,new.data,light.cal,fit)

#####################
#####################3

###################
## Data data 
## 
##

datfile <- paste(label,".dat", sep = "")
source(paste(srcDir,"CreateDat.r",sep=""))


	
###################
####  Environmental data
##

#envfile <- paste(label,".env",sep = "")
#if(file.exists(tabfile)) source("CreateEnv.r")

##### End create data files
#############################################################################




############################################################################
#### 
#### Environmental masks
#envfile <- paste(label,".env",sep = "")

#data.env <- read.table(envfile,header=T)
#data.env$gmt <- as.POSIXct(strptime(paste(data.env$date,data.env$time),
  #                           "%d-%m-%Y %H:%M:%S"),"GMT")

#datfile <- paste(label,".dat",sep="")

## load "masks"

## see C:\Documents and Settings\mdsumner\My Documents\PROJECTS\spatialMatrix\topoSstMask25Mar04
#load("masks29Mar04.Rdata")
#load("EnvMasks16Apr.Rdata")


###########################################################################
############################################################################


###########################################################################
############################################################################
####
#### Prior, posterior and metropolis

## in metropolis() the call to chol has pivot = T (required in 1.7.0, may be OK in earlier?)

# source("metro.r")

####
#### Initialize the chain

## this is MkElevationSeg (elev.r is without segments)

# source("elevations.r")
data <- read.table(datfile, header = T)
data$gmt <- as.POSIXct(strptime(paste(data$date,data$time),
                            "%d-%m-%Y %H:%M:%S"),"GMT") 

nlogpost <- mkNLPosterior(data$segment,data$gmt,data$light)

## For each segment find pixel with minimum nlogpost

#source("InitChain.r",echo = T)

## Make a matrix to store the parameters. Although storing sets of
## parameters as rows is more natural, it is less efficient later.
npars <- 3
nsegs <- length(unique(data$segment))
ps.mode <- matrix(0,nsegs,npars)
colnames(ps.mode) <- c("Lat","Lon","k")
## For each segment find pixel with minimum nlogpost
for(k in 1:nsegs) {
  cat(k,"\n")
  lat.range <- range(masks$y)
  lon.range <- range(masks$x)
  
  Xcell <- ((lon.range[2]-lon.range[1])/dim(masks$z)[2])/2
  Ycell <- ((lat.range[2]-lat.range[1])/dim(masks$z)[1])/2

  ## This is the middle of the grid cell
  p <- c(lat.range[1]+Ycell,lon.range[1]+Xcell,0)
  nlp.mode <- nlogpost(k,p,p,p) 
  p.mode <- p
  #points(p.mode[2],p.mode[1])
  ## Loop over centers of pixels
  for(lat in lat.range[1]:(lat.range[2]-1)) {
    for(lon in lon.range[1]:(lon.range[2]-1)) {
      if(test.mask(masks,k,lat,lon)==1) {
        p <- c(lat+Ycell,lon+Xcell,0)
        nlp <- nlogpost(k,p,p,p)
        if(nlp < nlp.mode) {
          nlp.mode <- nlp
          p.mode <- p

        }
      }
      
    }
  }
  ps.mode[k,] <- p.mode
}



#### Run it

summ <- sMat(dimYX=c(100,100),xlim=range(masks$x),ylim=range(masks$y))

par(mfrow = c(3,3))
source(paste(srcDir,"RunChain.r",sep=""))




########################################################################
########################################################################
##
## Summarize results
##

## Simplest approach is to plot individual points
par(mfrow=c(3,5))
for(i in 1:nsegs) {
  lats <- ch$p[,(i-1)*npars+2]
  lons <- ch$p[,(i-1)*npars+1]
  plot(lats,lons,
       xlab="lon",ylab="lat",main=i,pch=".",
       xlim=c(lon.min,lon.max),ylim=c(lat.min,lat.max))
}

## Better approach is 2d kernel density estimation (as discussed by
## Simonoff).
library(MASS)
par(mfrow=c(1,1))

contour(kde2d(lons,lats,n=50),xlim=xrange,ylim=yrange,add =F)
for(i in 1:nsegs) {
  lats <- ch$p[,(i-1)*npars+1]
  lons <- ch$p[,(i-1)*npars+2]
  contour(kde2d(lons,lats,n=50),add =F,xlim=xrange,ylim=yrange)
  lines(d[,2],d[,1],col="red")
  #dev.print(png, file=paste("SST_depth_",i,".png",sep = ""), width=400, height=400)
	
}

## Better approach is 2d kernel density estimation (as discussed by
## Simonoff).
library(MASS)
par(mfrow=c(1,1))
xrange = c(143,159)
yrange=c(-68,-53)
  lats <- ch$p[,(1-1)*npars+1]
  lons <- ch$p[,(1-1)*npars+2]
for(i in 2:nsegs) {
  lats <- c(lats,ch$p[,(i-1)*npars+1])
  lons <- c(lons,ch$p[,(i-1)*npars+2])
  
}
contour(kde2d(lons,lats,n=500),xlim=xrange,ylim=yrange,add =F)



## Text summary
apply(ch$p,2,
      function(x) c(mn=mean(x),
                    sd=sqrt(var(x)),
                    quantile(x,prob=c(0.5,0.025,0.975))))


## SST check

lats <- lat.min:lat.max
lons <- lon.min:lon.max
sst <- get.mask(masks,1)
image(lats,lons,sst)
points(ch$p[,1],ch$p[,2])

library(MASS)
##output
par(mfrow=c(1,1))
 for(i in 1:nsegs) {
   lons <- ch$p[,(i-1)*npars+2]
   lats <- ch$p[,(i-1)*npars+1]
   if (i == 1) {
	longitude <- lons
	latitude  <- lats
	 segment <- rep(i,length(lons))
   } else {
	longitude <- c(longitude,lons)
	latitude <- c(latitude,lats)
	segment <- c(segment,rep(i,length(lons)))
   }
  # plot(lons,lats,
   #     xlab="lon",ylab="lat",main=i,pch=".",
    #    xlim=c(lon.min,lon.max),ylim=c(lat.min,lat.max))


 }
  contour(kde2d(longitude,latitude,n=50),xlim = xrange, ylim = yrange,add = F)
  dev.print(png, file=paste("SST_depth_",i,".png",sep = ""), width=400, height=400)

write.table(data.frame(lons = longitude,lats = latitude,segment = segment), "Real.csv",sep = ",",row.names = F)









