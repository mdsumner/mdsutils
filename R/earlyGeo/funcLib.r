####
#### elevations.r

## mkElevationSeg


####
#### metro.r

## prior
## dist.gc
## mkNLposterior
## metropolis

####
####  elev.r

## julday
## mkElevation






####
#### elevations.r

## mkElevationSeg


mkElevationSeg <- function(segments,day) {
  ## day - times as POSIXct
  ## segments - indexs separate segments of light curve 
  
  ## Extract components of time (GMT)
  tm <- as.POSIXlt(day,tz="GMT")           
  hh <- tm$hour
  mm <- tm$min
  ss <- tm$sec


  ## Time as Julian day
  jday <- julday(tm)+(hh+(mm+ss/60)/60)/24
  
  ## Time as Julian century
  t <- (jday-2451545)/36525

  ## Geometric mean anomaly for the sun (degrees)
  M <- 357.52911+t*(35999.05029-0.0001537*t)

  ## Equation of centre for the sun (degrees)
  eqcent <- sin(pi/180*M)*(1.914602-t*(0.004817+0.000014*t))+
    sin(pi/180*2*M)*(0.019993-0.000101*t)+
      sin(pi/180*3*M)*0.000289
    
  ## The geometric mean sun longitude (degrees)
  L0 <- 280.46646+t*(36000.76983+0.0003032*t)
  ## Limit to [0,360)
  L0 <- L0%%360 

  ## The true longitude of the sun (degrees)
  lambda0 <- L0 + eqcent
  
  ## The apparent longitude of the sun (degrees)
  omega <- 125.04-1934.136*t
  lambda <- lambda0-0.00569-0.00478*sin(pi/180*omega)
  

  ## The mean obliquity of the ecliptic (degrees)
  seconds <- 21.448-t*(46.815+t*(0.00059-t*(0.001813)))
  obliq0 <- 23+(26+(seconds/60))/60 

  ## The corrected obliquity of the ecliptic (degrees)
  omega <- 125.04-1934.136*t
  obliq <- obliq0 + 0.00256*cos(pi/180*omega)

  ## The eccentricity of earth's orbit
  e <- 0.016708634-t*(0.000042037+0.0000001267*t)
  
  ## The equation of time (minutes of time)
  y <- tan(pi/180*obliq/2)^2
  eqtime <- 180/pi*4*(y*sin(pi/180*2*L0) -
                      2*e*sin(pi/180*M) +
                      4*e*y*sin(pi/180*M)*cos(pi/180*2*L0) -
                      0.5*y^2*sin(pi/180*4*L0) -
                      1.25*e^2*sin(pi/180*2*M))

  ## The sun's declination (radians)
  solarDec <- asin(sin(pi/180*obliq)*sin(pi/180*lambda))
  sinSolarDec <- sin(solarDec)
  cosSolarDec <- cos(solarDec)

  ## ALT
  ## sinSolarDec <- sin(pi/180*obliq)*sin(pi/180*lambda)
  ## cosSolarDec <- cos(asin(sinSolarDec))

  ## Solar time unadjusted for longitude (degrees)
  solarTime <- (hh*60+mm+ss/60+eqtime)/4

  ## Split by segment
  solarTime <- split(solarTime,segments)
  sinSolarDec <- split(sinSolarDec,segments)
  cosSolarDec <- split(cosSolarDec,segments)
  
  function(segment,lat,lon) {
  
    ## Suns hour angle (degrees)
 ## change subtraction to addition to work with -180<->180 convention MDS2Jul03
   
    hourAngle <- solarTime[[segment]]+lon-180
    
    ## Cosine of sun's zenith 
    cosZenith <- sin(pi/180*lat)*sinSolarDec[[segment]]+ 
      cos(pi/180*lat)*cosSolarDec[[segment]]*cos(pi/180*hourAngle)
    
    ## Limit to [-1,1] 
    cosZenith[cosZenith > 1] <- 1
    cosZenith[cosZenith < -1] <- -1
    
    ## Ignore refraction correction
    90-180/pi*acos(cosZenith) 
  }
}



####
#### metro.r

## prior
## dist.gc
## mkNLposterior
## metropolis


########################################################################
########################################################################
##
## Prior and Posterior
##

## The prior really has two components, the enviromental masks + any
## other prior we wish to adopt.  Because we tend to work with log
## densities, it is necessary to keep these separate to avoid problems
## with log 0.


## Put a mildly restrictive prior on "k"
prior <- function(seg,ps) dnorm(ps[3],0,10)


## Great circle distance from one (lat,lon) point to another
dist.gc <- function(pt1,pt2) {
  ## Assume pts are in the form (lat,lon)
  ## this has been checked:  MSumner 11July2003

## pt1 <- c(-54.5562,158.9178)
## pt2 <- c(-53.8988,156.3170)
## dist.gc(pt1,pt2)
## [1] 184.3738
## Manifold - 184.3738
## pt1 <-c(-47.9102,159.0042)
## pt2 <- c(-58.2751,150.0789)
## dist.gc(pt1,pt2)
## [1] 1296.411
## Manifold - 1296.4112

 6378.137*acos(sin(pi/180*pt1[1])*sin(pi/180*pt2[1])+
                   cos(pi/180*pt1[1])*cos(pi/180*pt2[1])*
                   cos(pi/180*(pt2[2]-pt1[2])))

}


## mkNLPosterior constructs a function that returns the contribution
## to the negative log posterior from a given individual light curve
## segment. There are a few tricky implementation points here.  We
## represent the parameters as a vector whose first two components are
## the (lat,lon) coords which can then be passed transparently to
## functions such as gcdist.  For efficiency we precompute as much of
## the elevation calculation as possible, and pre-split the data into
## the different light curve segments for easy access later.

## The likelihood itself has two components, one arising from the fit
## of the light curve, and the other from the distance from the
## previous and next points on the track.  In this simplementation we
## assume that the data are Normally distributed around the true light
## curve, and that the dsitrubtion of distances travelled is gamma,
## and independent of the actuall "time" between segments.

## CHECK - the calls to split actually work.

mkNLPosterior <- function(segments,day,light) {

  segments <- unclass(factor(segments))
  
  ## Construct elevation function
  elevation <- mkElevationSeg(segments,day)

  ## Split light levels by segments
  light <- split(light,segments)
  
  ## We return a function that computes the negative log posterior
  function(seg,ps,prv,nxt) {
    ## seg - the segment of the light curve
    ## ps - params for this segment
    ## prv,nxt - params for previous and next segments

    ## Decompose params into (lat, lon) and offset
    lat <- ps[1]
    lon <- ps[2]
    k <- ps[3]
	
##-  Problem sometimes with elevation - "trying to subscript too many elements"
##- (might be the split)
    ## Compute elevations for this (lat, lon) and segment
    elev <- elevation(seg,lat,lon)

    ## The expected light levels
    lgt <- calib(elev)
    
    ## The log likelihood.
    sigma <- 7
    shape <- (70/30)^2
    rate <- 70/30^2
    eps <- 1.0E-6
    loglik <- sum(dnorm(light[[seg]],k+lgt,7,log=T),
                  ## Must not allow zero distances for gamma pdf.
                  dgamma(max(eps,dist.gc(prv,ps)),shape,rate,log=T),
                  dgamma(max(eps,dist.gc(ps,nxt)),shape,rate,log=T))

    ## Return negative log posterior
    -(log(prior(seg,ps))+loglik)
  }
}

metropolis <- function(nlpost,mask,p0,cov0,start,end,sMat,iter=1000,step=100) {

  ## Initialize chain - internally we always work with transpose p.
  p <- t(p0)
  
  ## m parameters per twilight and n twilights
  m <- nrow(p)
  n <- ncol(p)

  ## Precalculate the Cholesky decomposition the covariance of the
  ## proposal distribution for each block.
  U <- cov0
  for(j in 1:n) U[,,j] <- chol(U[,,j],pivot = T)	## I think you need pivot for 1.7.0 

  ## Record chain as a matrix
  chain.p <- matrix(0,iter,m*n)
  colnames(chain.p) <- paste(colnames(p0),rep(1:n,each=m),sep="")
  
  ## Posterior for each block at initial locations
  l <- rep(0.0,n)
  l[1] <- nlpost(1,p[,1],start,p[,2])
  for(j in 2:(n-1)) {
    l[j] <- nlpost(j,p[,j],p[,j-1],p[,j+1])
  }
  l[n] <- nlpost(n,p[,n],p[,n-1],end)
  
  for(i in 1:iter) {
    for(k in 1:step) {
      
      ## First location compares to start
      pj.k <- p[,1] + rnorm(m) %*% U[,,1]

      ## CHECK this use of test.mask


      if(test.mask(masks,1,pj.k[1],pj.k[2])) {
        lj.k <- nlpost(1,pj.k,start,p[,2])
        ## Test candidate against l for this segment
        if(l[1]-lj.k > log(runif(1))) {
          ## accept candidate
          p[,1] <- pj.k
          l[1] <- lj.k
      
	sMat <- increMat(sMat,c(pj.k[1],pj.k[2]))
        }
      }
      
      ## Middle locations
      for(j in 2:(n-1))  {
        pj.k <- p[,j] + rnorm(m) %*% U[,,j]
        if(test.mask(masks,j,pj.k[1],pj.k[2])) {
          lj.k <- nlpost(j,pj.k,p[,j-1],p[,j+1])
          if(l[j]-lj.k > log(runif(1))) {
            ## accept candidate
            p[,j] <- pj.k
            l[j] <- lj.k
	sMat <- increMat(sMat,c(pj.k[1],pj.k[2]))
          }
        }
      } 
      
      ## Last location compares to end
     #???pj.k <- p[,n] + rnorm(n,n,m) %*% U[,,n]
	 pj.k <- p[,n] + rnorm(m) %*% U[,,n]
      if(test.mask(masks,n,pj.k[1],pj.k[2])) {
        lj.k <- nlpost(n,pj.k,p[,n-1],end)
        if(l[n]-lj.k > log(runif(1))) {
          ## accept candidate
          p[,n] <- pj.k
          l[n] <- lj.k
	sMat <- increMat(sMat,c(pj.k[1],pj.k[2]))
        }
      }
    
    }
    chain.p[i,] <- p
    print(i)
  }

  p <- t(p)
 # names(p) <- names(p0)
  list(p=chain.p,last=p,sMat=sMat)
 
}


#####################
#####################

####
####  elev.r

## julday
## mkElevation

julday <- function(tm) {
  
  yr <- 1900+tm$year
  mon <- tm$mon+1
  day <- tm$mday

  yr[mon<=2] <- yr[mon<=2]-1
  mon[mon<=2] <- mon[mon<=2]+12

  a <- floor(yr/100)
  b <- 2-a+floor(a/4)
  floor(365.25*(yr+4716)+floor(30.6001*(mon+1)))+day+b-1524.5
}


mkElevation <- function(day) {

    ## day - times as POSIXct
  
  ## Extract components of time (GMT)
  tm <- as.POSIXlt(day,tz="GMT")           
  hh <- tm$hour
  mm <- tm$min
  ss <- tm$sec


  ## Time as Julian day
  jday <- julday(tm)+(hh+(mm+ss/60)/60)/24
  
  ## Time as Julian century
  t <- (jday-2451545)/36525

  ## Geometric mean anomaly for the sun (degrees)
  M <- 357.52911+t*(35999.05029-0.0001537*t)

  ## Equation of centre for the sun (degrees)
  eqcent <- sin(pi/180*M)*(1.914602-t*(0.004817+0.000014*t))+
    sin(pi/180*2*M)*(0.019993-0.000101*t)+
      sin(pi/180*3*M)*0.000289
    
  ## The geometric mean sun longitude (degrees)
  L0 <- 280.46646+t*(36000.76983+0.0003032*t)
  ## Limit to [0,360)
  L0 <- L0%%360 

  ## The true longitude of the sun (degrees)
  lambda0 <- L0 + eqcent
  
  ## The apparent longitude of the sun (degrees)
  omega <- 125.04-1934.136*t
  lambda <- lambda0-0.00569-0.00478*sin(pi/180*omega)
  

  ## The mean obliquity of the ecliptic (degrees)
  seconds <- 21.448-t*(46.815+t*(0.00059-t*(0.001813)))
  obliq0 <- 23+(26+(seconds/60))/60 

  ## The corrected obliquity of the ecliptic (degrees)
  omega <- 125.04-1934.136*t
  obliq <- obliq0 + 0.00256*cos(pi/180*omega)

  ## The eccentricity of earth's orbit
  e <- 0.016708634-t*(0.000042037+0.0000001267*t)
  
  ## The equation of time (minutes of time)
  y <- tan(pi/180*obliq/2)^2
  eqtime <- 180/pi*4*(y*sin(pi/180*2*L0) -
                      2*e*sin(pi/180*M) +
                      4*e*y*sin(pi/180*M)*cos(pi/180*2*L0) -
                      0.5*y^2*sin(pi/180*4*L0) -
                      1.25*e^2*sin(pi/180*2*M))

  ## The sun's declination (radians)
  solarDec <- asin(sin(pi/180*obliq)*sin(pi/180*lambda))
  sinSolarDec <- sin(solarDec)
  cosSolarDec <- cos(solarDec)

  ## ALT
  ## sinSolarDec <- sin(pi/180*obliq)*sin(pi/180*lambda)
  ## cosSolarDec <- cos(asin(sinSolarDec))
  
  function(lat,lon) {
  
    ## Assume we always work GMT.

  ## change subtraction to addition to work with -180<->180 convention MDS2Jul03
    solarTimeFix <- eqtime+4*lon
    
    ## True solar time (minutes)
    trueSolarTime <- hh*60+mm+ss/60+solarTimeFix
    
    ## Limit to [0,1440)
    trueSolarTime <- trueSolarTime%%1440
    
    ## Suns hour angle (degrees)
    hourAngle <- trueSolarTime/4-180
    
    ## Limit to [-180,180) (?? is this necessary ??)
    hourAngle <- ((hourAngle+180)%%360)-180

    ## ALT
    ## cosHA <- cos(pi/180*(trueSolarTime/4-180))
    
    ## Cosine of sun's zenith 
    cosZenith <- sin(pi/180*lat)*sinSolarDec+ 
      cos(pi/180*lat)*cosSolarDec*cos(pi/180*hourAngle)
    
    ## Limit to [-1,1] 
    cosZenith[cosZenith > 1] <- 1
    cosZenith[cosZenith < -1] <- -1
    
    zenith <- 180/pi*acos(cosZenith) 
    ## Estimate refraction correction
    elev <- 90-zenith
    te <- tan(pi/180*elev)
    refraction <- ifelse(elev > 85,
                         0,
                         ifelse(elev > 5,
                                58.1/te - 0.07/te^3 + 0.000086/te^5,
                                ##(58.1 + (-0.07 + 0.000086/te^2)/te^2)/te,
                                ifelse(elev > -0.575,
                                       1735+elev*(-518.2+elev*(103.4+elev*(-12.79+elev*0.711))),
                                       -20.774/te)))/3600
    
    elev <- 90-(zenith-refraction)
    elev
  }
}


#lat <- -42.8666666666666	
#lon <- -147.31666666666666
#d <- ISOdatetime(2003, 3, 23, 2, 48, 53, tz = "GMT")
#f <- mkElevation(d)
#f(lat,lon)



#lat <- -68.8666666666666	
#lon <- -120.31666666666666
#d <- ISOdatetime(2003, 3, 23, 12, 48, 53, tz = "GMT")
#f <- mkElevation(d)
#f(lat,lon)



#lat <- -42.8666666666666	
#lon <- -147.31666666666666
#d1 <- ISOdatetime(2000, 1, 20, 14, 12, 07, tz = "GMT")
#d2 <- ISOdatetime(2000, 2, 20, 14, 12, 07, tz = "GMT")
#d3 <- ISOdatetime(2000, 3, 20, 14, 12, 07, tz = "GMT")
#ds <- c(d1,d2,d3)

#f <- mkElevation(ds)
#f(lat,lon)

## END elev.r
#####################
#####################

