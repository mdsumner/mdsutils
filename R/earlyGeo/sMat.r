## sMat
##     - spatial matrix functions for R
##
##  Michael D. Sumner March 16 2004
##
##  sMat is a bunch of R functions for creating, modifying
##   and displaying image data
##   - an sMat object is a list with components x, y, z
##
##  Usage:
##    source("sMat.r")
##    mat <- sMat()
##    image(mat)
##
##  References:
##     R "base" for image, contour etc.
##     Martin Biuw for reading SS topography
##
##  Revision history:
##   - changed to xyz configuration, MDS 5 April 2004
##   - reverted to R's default matrix/image orientation,
##     updated all functions, improvements here and there MDS 13 May 2004
##
##  Functions:
##
## sMat - create a spatial matrix as a list of "x","y","z"
##
## updateMat - batch update a matrix with a 1 added to pixel value for
##            each world coordinate point

## ####SUPERCEDED by updateMat -increMat  - add values to a sMat pixel
##
## image.sMat - display a sMat matrix
##
## contour.sMat - add contours to existing display
## 
## rebinMat - resize a matrix by subsequencing
## 
## xmlBin - output a sMat matrix to binary with (Manifold) XML

## TODO:
## - maybe add "world2" = T as a list object vs. "world"
## - complete design for array masks handling

sMat <- function(z=NULL,xlim=c(-180,180),ylim=c(-90,90),dimXY=dim(z),
                 array=FALSE,...)
{
  if (missing(z)) {
    if (is.null(dimXY)) {
      dimXY <- c(36,18);
  }
    z <- matrix(0,dimXY[1],dimXY[2],byrow=F);
  } else {
    if (array) {
      if(!is.array(z))
        stop("z must be an array of  matrix masks with array = TRUE");
    } else {
    if (!is.matrix(z))
      stop("z must be a matrix (or an array of matrix masks with array = TRUE)");
  }
  }
  if (length(xlim) < 2 | length(ylim) < 2)
    stop("xlim and ylim must be vectors of length 2");
  #cat(xlim,"\n",ylim,"\n",dimYX,"\n")
  localScaleX <- (xlim[2]-xlim[1])/dimXY[1];
  localScaleY <- (ylim[2]-ylim[1])/dimXY[2];
  if (localScaleX <= 0 | localScaleY <= 0)
    stop("positive x and y pixel size expected in xlim and ylim");
  localOffsetX<- xlim[1];
  localOffsetY<- ylim[1];
  ## these are cell boundaries
  xs <- seq(localOffsetX,length=dimXY[1]+1,by=localScaleX);
  ys <-  seq(localOffsetY,length=dimXY[2]+1,by=localScaleY);
  mat <- list(x=xs,y=ys,z=z);
  if (!any(class(mat) == "sMat")) class(mat) <- c(class(mat),"sMat");
  mat;
}
updateMat <- function(mat,wps) {
  ## coordinate system
  orig <- c(mat$x[1],mat$y[1])
  scl <- c(diff(mat$x)[1],diff(mat$y)[1])
  xdim <- dim(mat$z)[1]
  ydim <- dim(mat$z)[2]
  xmin <- min(mat$x)
  xmax <- max(mat$x)
  ymin <- min(mat$y)
  ymax <- max(mat$y)
  ## pixel coordinates
  cps <- ceiling(cbind((wps[,1] - orig[1])/scl[1],(wps[,2] - orig[2])/scl[2]))
  ## tabulate pixel coordinates, using matrix size
  tps <- tabulate((cps[,1]-1)*ydim + cps[,2],xdim*ydim)
  ## coerce table to matrix
  mps <- matrix(tps,ydim,xdim)
  ## create sMat with t(matrix)
  mat2 <- sMat(t(mps),xlim=c(xmin,xmax),ylim=c(ymin,ymax))
}
image.sMat <- function(x,k=NULL,norescale=FALSE,grid=FALSE,...)
{ ## catch matrices that are too big
  if (max(dim(x$z)) > 800 & !norescale) {
    x <- rebinMat(x,subs=max(dim(x$z)/600));
    cat("matrix resized for display to ", dim(x$z), "\n")
    cat(" - use \"norescale\" keyword to prevent this\n")
  }
  #xs <- x$x
  #ys <- x$y
  # zz <- x$z
  ## this line is for retrieving a mask from the bit mask arrays
  if (is.array(x$z) && !is.matrix(x$z)){
    if (is.null(k)) {
      cat("Specify mask number to display from array k=\n");
      k <- scan(n=1,quiet=TRUE);
    }
    zz <- get.mask(x,k);
  }
  ## transpose and reverse for display
  ## xx <- as.matrix(rev(as.data.frame(t(zz))));
  ##image(xs,ys,xx,...)
  image.default(x,...)
  if (grid) {grid(length(x$x)-1,length(x$y)-1)}
}
contour.sMat <- function(x,k=1,norescale=FALSE,...) {
  ## this line is for retrieving a mask from the bit mask arrays
  if (length(dim(x)) > 2)  x <- get.mask(x,k)
  ## catch matrices that are too big
  if (max(dim(x$z) > 2000) & !norescale) {
    x <- rebinMat(x,subs=max(dim(x$z)/1500));
      cat("matrix resized for display to ", dim(x$z), "\n")
    cat(" - use \"norescale\" set to TRUE to prevent this\n")
  }
  #xx <- as.data.frame(t(x$z)); 
  #xx <- rev(xx); 
  #xx <- as.matrix(xx);
  xx <- x$z
  xs <- x$x;
  ys <- x$y;
  ## catch out when the x/ys are cell boundaries
  if (length(x$x) > dim(x$z)[2]) {
    xs <- x$x[-length(x$x)] + (x$x[2] - x$x[1])/2;
    ys <- x$y[-length(x$y)] + (x$y[2] - x$y[1])/2;
 }
  contour.default(xs,ys,xx,add=T,...)
}
range.sMat <- function(x,...) {
  cat("\nRange\nX:   ",range.default(x$x))
  cat("\nY:   ",range.default(x$y))
  cat("\nZ:   ",range.default(x$z),"\n")
}
dim.sMat <- function(x,...) {
  cat("\nLength\nX:   ",length(x$x))
  cat("\nY:   ",length(x$y))
  cat("\nDimension\nZ:   ",dim(x$z),"\n")
}
rebinMat <- function(x,subs=4) {
  ## this should work in either direction - uses subsequencing
  if(any(dim(x$z)-1 < subs))
    stop("subsequence factor must be less than matrix dimension")
  rbmat <- x$z[seq(1,nrow(x$z),by=subs),seq(1,ncol(x$z),by=subs)]
  xRange <- c(x$x[1],x$x[length(x$x)])
  yRange <- c(x$y[1],x$y[length(x$y)])
  sMat(z=rbmat,xlim=xRange,ylim=yRange)
}
increMat<- function(x,xy,incr=1,quiet=FALSE,debug=FALSE) {
  ## take a matrix (x) and a coordinate (yx, c(lat,lon)),
  ## add one to cell containing point
  indX <- max(which(x$x <= xy[1]))
  #indY <- max(which(rev(x$y) >= yx[1]))
  indY <- max(which(x$y <=xy[2]))
  if (debug) {cat("X: ",indX,"\n","Y: ",indY,"\n" )}
  if (!(indX > (length(x$x)-1) | indY > (length(x$y)-1))){
	  x$z[indX,indY] =  x$z[indX,indY] + incr
  }else if (!quiet) cat("Point is not within bounds of matrix, no cell values incremented\n")
  x
}
queryMat<- function(x,xy,incr=1,quiet=FALSE) {
  ## take a matrix (x) and a coordinate (yx, c(lat,lon)),
  ## find the matching matrix value

  indX <- max(which(x$x <= xy[1]))
  #indY <- max(which(rev(x$y) >= yx[1]))
  indY <- max(which(x$y <=xy[2]))

  if (!(indX > (length(x$x)-1) | indY > (length(x$y)-1))){
	x$z[indX,indY] 
  }else if (!quiet) {
	cat("Point is not within bounds of matrix, no value to return\n")
	NA
 	}
} 
swap.dl <- function(x) {
 # west <- xs < 0
 # if(any(west))
 #   xs[west] <- xs[west] + 360
  ## swap a matrice's E-W hemispheres - assuming about the dateline/greenwich
  cat("WARNING: expert use only!\n")
  cat(" use swap.dl with caution \n")
  #cols <- ncol(x)
  #x2 <- x
  #if (!identical(cols/2,floor(xNum/2))) return("!!")
  zz <- x$z
  xNum <- nrow(zz)
  xx <- x$x
  xx[1:(xNum/2)]  <- x$x[(xNum/2+1):xNum]
  xx[(xNum/2+1):xNum]  <- x$x[1:(xNum/2)] + 360
  zz[1:(xNum/2),]  <- x$z[(xNum/2+1):xNum,]
  zz[(xNum/2+1):xNum,]  <- x$z[1:(xNum/2),]
  sMat(zz,xlim=c(min(xx),max(xx)))
}
region <- function(x,xlim=c(-180,180),ylim=c(-90,90)) {
  xs <- x$x[-length(x$x)]
  rngX <- xs >= xlim[1] & xs <= xlim[2]
  ys <- x$y[-length(x$y)]
  rngY <- ys >= ylim[1] & ys <= ylim[2]
  sMat(z = x$z[rngX,rngY],xlim=xlim,ylim=ylim)
}
pentad.DOY <- function(x) {
  ## Pentad 1 is centered on January 3, pentad 2 is centered on January 8
  ## and so forth.
     ## what pentad (every five days of a year) does a date fall into?
    ## there are 73 per year
    ## sanity check
    if (!any(class(x)=="POSIXct")) 
	return("Need as POSIXct objects")
     ##cat("pentad determination not fully tested - beware\n")
	ceiling(as.POSIXlt(x)$yday / 5)
}
read.PFpentad <- function(convert=TRUE) {
  nrows <- 2048
  ncols <- 4096
  lon.min <- -180
  lon.max <- 180
  lat.min <- -90
  lat.max <- 90
  slope <- 0.15;intercept<--3.0;
  if (!convert) {slope<-1;intercept<-0;}
  filename <- "C:/Documents and Settings/mdsumner/My Documents/DATA/pathfinderCLIM/jpl_pentad.1985-1999"
  function(pK) {
    conn <- file(filename,open = "rb")
    ByteSkip <- 76 + pK*nrows*ncols
    seek(conn,ByteSkip,rw="r")
    d <- readBin(conn,"int",n=nrows*ncols,signed=F,size=1)
    seek(conn,0,rw="r")
    sst <- matrix(data = d*slope+intercept, nrow = nrows,ncol= ncols,byrow = TRUE)
    close(conn)
    sMat(z = as.matrix(rev(as.data.frame(t(sst)))));
  }
}
read.etopo2 <- function(filename=NULL) {
  nrows <- 2048
  ncols <- 4096
  lon.min <- -180
  lon.max <- 360
  lat.min <- -90
  lat.max <- 90
  pentads <- 73
  if (is.null(filename))
    filename <- "C:/Documents and Settings/mdsumner/My Documents/DATA/etopo/etopo4096x2048.bin"
  topo <- readBin(filename,"numeric",n=4096*2048,size=4,endian="big")
  sMat(as.matrix(rev(as.data.frame(t(matrix(topo,nrow=nrows,byrow=T))))))
}
read.Heard <- function(filename=NULL) {
  nrows <- 900
  ncols <- 600
  lon.min <- 65.02269
  lon.max <- 94.99213
  lat.min <- -64.99537
  lat.max <- -45.02500
  if (is.null(filename))
    filename <- "HeardTopo.bin"
  topo <- readBin(filename,"numeric",n=nrows*ncols,size=8,endian="little")
  sMat(z = matrix(topo,nrow=nrows,byrow=F),xlim=c(lon.min,lon.max),ylim=c(lat.min,lat.max))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Smith&Sandwell.R

subset.SS <- function(filename=NULL,xlim=c(0,360),ylim=c(-90,90),byrow=TRUE) {
  ## I've modified this to not save by default, to check for
  ## netCDF support, and to return an sMat object MDSumner 13 May 2004      
	require(netCDF) || stop("no netCDF support!")
        if (is.null(filename))
          filename = "C:/Documents and Settings/mdsumner/My Documents/DATA/etopo/SMITH_SANDWELL_TOPO_2MIN.nc"
	SS 	<- open.netCDF(filename)
	ys	<- read.netCDF(SS, id=1)
	xs	<- read.netCDF(SS, id=0)	
		
        rngX <- which(xs >= xlim[1] & xs <= xlim[2])
        rngY <- which(ys >= ylim[1] & ys <= ylim[2])
	
	YN	<- max(rngY)
	YS	<- min(rngY)
	XW 	<- min(rngX)
	XE 	<- max(rngX)
	
	cdf 	<- read.netCDF(SS, id=3, 
		    start=c(YS, XW), 
		    count=c(XE-(XW-1),YN-(YS-1)), 
		    byrow=byrow, attr=TRUE)
        cdf <- mirror.matrix(rotate270.matrix(cdf))
	close.netCDF(SS)
	sMat(z=cdf,ylim=range(ys[rngY]),xlim=range(xs[rngX]))
}
## Given the array of packed masks this function tests whether a
## particular (lat,lon) "feasible" in the k-th mask.
test.mask <- function(x,k,lat,lon) {
  localOffsetX <- min(x$x)
  localScaleX <- diff(x$x)[1]
  localOffsetY <- min(x$y)
  localScaleY <- diff(x$y)[1]
  dimY <- dim(x$z)[1]
  dimX <- dim(x$z)[2]
  if (
      (lat > localOffsetY && lat < (localOffsetY+dimY*localScaleY)) && 
      (lon > localOffsetX && lon < (localOffsetX + dimX*localScaleX))
      )  {
    indX <- floor((lon- localOffsetX)/localScaleX +1)
# remember to reverse y indices for image vs. coordinate origins
    indY <- dimY - floor((lat- localOffsetY)/localScaleY +1)  + 1
    bits(masks$z[indY,indX,(k%/%31)+1],(k-1)%%31)
  } else {0}
}
## Given the array of packed masks this function extracts the whole
## k-th mask.
get.mask <- function(masks,k) {
  ## Test the right bit.
  bits(masks$z[,,(k%/%31)+1],(k-1)%%31)
}
## Extract a specified bit from the integer object.
bits <- function(object,bit) {
  (object %/% (2^bit)) %% 2
}
## Set a specified bit in the integer object.
"bits<-" <- function(object,bit,value) {
  mask <- 2^bit
  object <- object+(value - ((object %/% mask) %% 2))*mask
  object
}
#################################################
##################################################

## xmlBin
############################################################################ 
# Matrix manipulation methods 
############################################################################ 
# Flip matrix (upside-down) 
flip.matrix <- function(x) { 
mirror.matrix(rotate180.matrix(x)) 
} 
# Mirror matrix (left-right) 
mirror.matrix <- function(x) { 
xx <- as.data.frame(x); 
xx <- rev(xx); 
xx <- as.matrix(xx); 
xx; 
} 
# Rotate matrix 90 clockworks 
rotate90.matrix <- function(x) { 
t(mirror.matrix(x)) 
} 
# Rotate matrix 180 clockworks 
rotate180.matrix <- function(x) { 
xx <- rev(x); 
dim(xx) <- dim(x); 
xx; 
} 
# Rotate matrix 270 clockworks 
rotate270.matrix <- function(x) { 
mirror.matrix(t(x)) 
}

#  x is a sMat matrix
xmlBin <- function(x,dataValue="val",bytesToSkip=0,binFile="test.bin",missing=-9999,midPix=F,type="numeric") {

# assumes 4 character extension
XMLfile <- gsub(".bin",".xml",binFile)

## fill the missing value
zz <- x$z
zz[is.na(zz) | is.null(zz) | !is.finite(zz)] <- missing

#if (any(class(x)=="sMat")){
#zz <- rotate90.matrix(zz)
zz <- mirror.matrix(zz)
#}
## write the z to binary
writeBin(as.vector(zz),binFile)

## write the metadata to xml
localScaleX <- x$x[2] - x$x[1]
localScaleY <-  x$y[2] - x$y[1]
localOffsetX <- x$x[1]
localOffsetY <- x$y[1] 
pixX <- nrow(x$z)
pixY <- ncol(x$z)
datatype <- R2ManifoldType(zz)
endian <- R2ManifoldEndian()

conn <- file(XMLfile,open="w")
cat("<xml>\n",sep="",file=conn)
cat("<preset>\n",sep="",file=conn)
cat("<name>",binFile,"</name>\n",sep="",file=conn)
cat("<category>Mike Sumner</category>\n",sep="",file=conn)
cat("<system>Latitude / Longitude</system>\n",sep="",file=conn)
cat("<datum>World Geodetic 1984 (WGS84)</datum>\n",sep="",file=conn)
cat("<localScaleX>",localScaleX,"</localScaleX>\n",sep="",file=conn)
cat("<localScaleY>",localScaleY,"</localScaleY>\n",sep="",file=conn)
cat("<unit>Degree</unit>\n",sep="",file=conn)
cat("<localOffsetX>",localOffsetX,"</localOffsetX>\n",sep="",file=conn)
cat("<localOffsetY>",localOffsetY,"</localOffsetY>\n",sep="",file=conn)
cat("<width>",pixX,"</width>\n",sep="",file=conn)
cat("<height>",pixY,"</height>\n",sep="",file=conn)
cat("<dataType>",datatype,"</dataType>\n",sep="",file=conn)
cat("<dataValue>",dataValue,"</dataValue>\n",sep="",file=conn)
cat("<missingValue>",missing,"</missingValue>\n",sep="",file=conn)
cat("<endian>",endian,"</endian>\n",sep="",file=conn)
cat("<bytesToSkip>",bytesToSkip,"</bytesToSkip>\n",sep="",file=conn)
cat("</preset>\n",sep="",file=conn)
cat("</xml>\n",sep="",file=conn)
close(conn)
}
## what is the data type in Manifold constants
R2ManifoldType<-function(x) {
	type <- typeof(x)
	switch(type,
		double="ValueTypeFloat64",
		integer="ValueTypeInt32"
	)
}
## Manifold endian
R2ManifoldEndian<-function(x=.Platform$endian) {
	ifelse(x=="little","LSB","MSB")
}


## possible data types from Manifold and IDL

#	;1 bit (IDL doesn,file=conn"t seem to have this)
#	,file=conn"BIT,file=conn": ManifoldType <- ,file=conn"ValueTypeBit,file=conn"

#	;Single precision floating-point number.
#	,file=conn"FLOAT,file=conn": ManifoldType <- ,file=conn"ValueTypeFloat32,file=conn"

#	;Double precision floating-point number.

## 	,file=conn"DOUBLE,file=conn": ManifoldType <- ,file=conn"ValueTypeFloat64

#	;16 bit integer
#	,file=conn"INT,file=conn":	 ManifoldType <- ,file=conn"ValueTypeInt16,file=conn"

#	;32 bit integer.
#	,file=conn"LONG,file=conn":	 ManifoldType <- ,file=conn"ValueTypeInt32,file=conn"

#	;64 bit integer.
#	,file=conn"LONG64,file=conn":ManifoldType <- ,file=conn"ValueTypeInt64,file=conn"

#	;8 bit integer. (IDL doesn,file=conn"t seem to have this)
#	,file=conn"SBYTE,file=conn":	 ManifoldType <- ,file=conn"ValueTypeInt8,file=conn"

#	;Unsigned 16 bit integer.
#	,file=conn"UINT,file=conn"	: ManifoldType <- ,file=conn"ValueTypeUInt16,file=conn"

#	;Unsigned 32 bit integer.
#	,file=conn"ULONG,file=conn"	: ManifoldType <- ,file=conn"ValueTypeUInt32,file=conn"

#	;Unsigned 64 bit integer.
#	,file=conn"ULONG64,file=conn"	: ManifoldType <- ,file=conn"ValueTypeUInt64,file=conn"

#	;Unsigned 8 bit integer.	
#	,file=conn"BYTE,file=conn"	: ManifoldType <- ,file=conn"ValueTypeUInt8,file=conn"


### palettes, yes the PF is overkill and the topo could be better, yes yes

## and maybe later these will be carried by sMat objects

## define some topographic colours and breaks for these data

ETcol <- c("#7897bb","#84afcf","#95c4dc","#bfdbe3","#e7eff1","#b6d294",
         "#d5e3b9","#f2dbb4","#e7bf83","#deac6b","#ab8e59","#e2ddd6",
         "#ebe7e2","#f2f0ee","#ffffff")
ETbrks <- c(-8000.00,-6000.00,-4000.00,-2000.00,-500.00,0.00,50.00,100.00,
          200.00,500.00,1000.00,2000.00,3000.00,4000.00,5000.00,6000.00)

## pathfinder:

PFcol<-c("#ffffff","#505050","#515151","#525252","#535353","#2a002a","#310031","#380038","#3f003f","#460046","#4d004d","#540054","#5b005b","#620062","#690069","#700070","#770077","#7e007e","#850085","#8c008c","#930093","#9a009a","#a100a1","#a800a8","#af00af","#b600b6","#bd00bd","#c400c4","#cb00cb","#d200d2","#d900d9","#e000e0","#e700e7","#ee00ee","#f500f5","#fc00fc","#f500fc","#ee00fc","#e700fc","#e000fc","#d900fc","#d200fc","#cb00fc","#c400fc","#bd00fc","#b600fc","#af00fc","#a800fc","#a100fc","#9a00fc","#9300fc","#8c00fc","#8500fc","#7e00fc","#7700fc","#7000fc","#6900fc","#6200fc","#5b00fc","#5400fc","#4d00fc","#4600fc","#3f00fc","#3800fc","#3100fc","#2a00fc","#2300fc","#1c00fc","#1500fc","#0e00fc","#0700fc","#0000fc","#0007fc","#000efc","#0015fc","#001cfc","#0023fc","#002afc","#0031fc","#0038fc","#003ffc","#0046fc","#004dfc","#0054fc","#005bfc","#0062fc","#0069fc","#0070fc","#0077fc","#007efc","#0085fc","#008cfc","#0093fc","#009afc","#00a1fc","#00a8fc","#00affc","#00b6fc","#00bdfc","#00c4fc","#00cbfc","#00d2fc","#00d9fc","#00e0fc","#00e7fc","#00eefc","#00f5fc","#00fcfc","#00fcf5","#00fcee","#00fce7","#00fce0","#00fcd9","#00fcd2","#00fccb","#00fcc4","#00fcbd","#00fcb6","#00fcaf","#00fca8","#00fca1","#00fc9a","#00fc93","#00fc8c","#00fc85","#00fc7e","#00fc77","#00fc70","#00fc69","#00fc62","#00fc5b","#00fc54","#00fc4d","#00fc46","#00fc3f","#00fc38","#00fc31","#00fc2a","#00fc23","#00fc1c","#00fc15","#00fc0e","#00fc07","#00fc00","#07fc00","#0efc00","#15fc00","#1cfc00","#23fc00","#2afc00","#31fc00","#38fc00","#3ffc00","#46fc00","#4dfc00","#54fc00","#5bfc00","#62fc00","#69fc00","#70fc00","#77fc00","#7efc00","#85fc00","#8cfc00","#93fc00","#9afc00","#a1fc00","#a8fc00","#affc00","#b6fc00","#bdfc00","#c4fc00","#cbfc00","#d2fc00","#d9fc00","#e0fc00","#e7fc00","#eefc00","#f5fc00","#fcfc00","#fcf500","#fcee00","#fce700","#fce000","#fcd900","#fcd200","#fccb00","#fcc400","#fcbd00","#fcb600","#fcaf00","#fca800","#fca100","#fc9a00","#fc9300","#fc8c00","#fc8500","#fc7e00","#fc7700","#fc7000","#fc6900","#fc6200","#fc5b00","#fc5400","#fc4d00","#fc4600","#fc3f00","#fc3800","#fc3100","#fc2a00","#fc2300","#fc1c00","#fc1500","#fc0e00","#fc0700","#fc0000","#f80000","#f40101","#f00201","#ec0302","#e80402","#e40503","#e00603","#dc0704","#d80804","#d40905","#d00905","#cc0a06","#c80b06","#c40c07","#c00d07","#bc0e08","#b80f08","#b41009","#b01109","#ac110a","#a8120a","#a4130b","#a0140b","#9c150c","#98160c","#94170d","#90180d","#8c190e","#881a0e","#841a0f","#801b0f","#7c1c10","#781d10","#741e11","#701f11","#6c2012","#682112","#642213","#602313","#000000")

PFbrks <- c(-3.00,-2.85,-2.70,-2.55,-2.40,-2.25,-2.10,-1.95,-1.80,-1.65,-1.50,-1.35,-1.20,-1.05,-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0.00,0.15,0.30,0.45,0.60,0.75,0.90,1.05,1.20,1.35,1.50,1.65,1.80,1.95,2.10,2.25,2.40,2.55,2.70,2.85,3.00,3.15,3.30,3.45,3.60,3.75,3.90,4.05,4.20,4.35,4.50,4.65,4.80,4.95,5.10,5.25,5.40,5.55,5.70,5.85,6.00,6.15,6.30,6.45,6.60,6.75,6.90,7.05,7.20,7.35,7.50,7.65,7.80,7.95,8.10,8.25,8.40,8.55,8.70,8.85,9.00,9.15,9.30,9.45,9.60,9.75,9.90,10.05,10.20,10.35,10.50,10.65,10.80,10.95,11.10,11.25,11.40,11.55,11.70,11.85,12.00,12.15,12.30,12.45,12.60,12.75,12.90,13.05,13.20,13.35,13.50,13.65,13.80,13.95,14.10,14.25,14.40,14.55,14.70,14.85,15.00,15.15,15.30,15.45,15.60,15.75,15.90,16.05,16.20,16.35,16.50,16.65,16.80,16.95,17.10,17.25,17.40,17.55,17.70,17.85,18.00,18.15,18.30,18.45,18.60,18.75,18.90,19.05,19.20,19.35,19.50,19.65,19.80,19.95,20.10,20.25,20.40,20.55,20.70,20.85,21.00,21.15,21.30,21.45,21.60,21.75,21.90,22.05,22.20,22.35,22.50,22.65,22.80,22.95,23.10,23.25,23.40,23.55,23.70,23.85,24.00,24.15,24.30,24.45,24.60,24.75,24.90,25.05,25.20,25.35,25.50,25.65,25.80,25.95,26.10,26.25,26.40,26.55,26.70,26.85,27.00,27.15,27.30,27.45,27.60,27.75,27.90,28.05,28.20,28.35,28.50,28.65,28.80,28.95,29.10,29.25,29.40,29.55,29.70,29.85,30.00,30.15,30.30,30.45,30.60,30.75,30.90,31.05,31.20,31.35,31.50,31.65,31.80,31.95,32.10,32.25,32.40,32.55,32.70,32.85,33.00,33.15,33.30,33.45,33.60,33.75,33.90,34.05,34.20,34.35,34.50,34.65,34.80,34.95,35.10,35.25,35.40)

SWcol <- c("#a046ff","#3760ff","#0095ff","#33c0ff","#68c7e8","#1ddd91","#02c20a","#4af100","#87ff00","#c5ff00","#f4ff00","#ffd600","#ffa000","#ff6a00","#ff1e00","#cd0000","#000000")


