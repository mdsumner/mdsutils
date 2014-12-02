## We input
## xs - vector of satellite locations (matrix n x 2)
## ts - times at which xs recorded (vector)
## First and last xs are known precisely.
##
## We want to estimate 
## ys - estimates of xs  (matrix n x 2)
## zs - latent points in between the ys (matrix n-1 x 2)
##
## grid.y - store ys into a grid
## grid.z - store zs into a grid
##
## We need to provide
## dspeed - the density of acceptable speeds
## dloc - the density of location (satellite) errors
##
## For the moment
## dloc - bivariate independent normal

## gc.dist - Great circle distances from (latA,lonA) to
## (latB,lonB). Does not correct for ellipticity of the earth.
gc.dist <- function(ptA,ptB) {
  ## Assume pts are in the form (lat,lon)
  ## answer returned is in nautical miles (1nm = 1.852km)
  3437.746771*acos(pmin(1,cos(pi/180*ptA[,1])*
                        cos(pi/180*ptB[,1])*
                        cos(pi/180*(ptB[,2]-ptA[,2]))+
                        sin(pi/180*ptA[,1])*sin(pi/180*ptB[,1])))
}


dloc <-  function(x,y,sigma) {
  ## Bivariate normal
  d <- x-y
  -(log(2*pi)+sum(log(sigma))+(d[,1]^2/sigma[1]+d[,2]^2/sigma[2]))
}

## For the moment speed is log normal
dspeed <- function(x) {
  dlnorm(x,4,1,log=T)
}

rbinorm <- function(mu,sigma) {
  matrix(rnorm(2,mu,sigma),1,2)
}

filter.mcmc <- function(x,sigma.l=c(1,1),sigma.y=c(0.1,0.1),sigma.z=c(0.1,0.1),
                        yMat=NULL,zMat=yMat,iters=20,kmod=50) {
                         
  n <- nrow(x)-1
  xs <- as.matrix(cbind(x$x,x$y))

  ## Time intervals
  ts.diff <- diff(x$t)
  
  if (!exists("sMat")) stop("Requires sMat functions!")
  if (is.null(yMat)) {
    xrange <- 0.1*diff(range(xs[,2]))*c(-1,1) + range(xs[,2])
    yrange <- 0.1*diff(range(xs[,1]))*c(-1,1) + range(xs[,1])
    yMat <- sMat(xlim =xrange,ylim = yrange,dimXY=c(100,100))
    zMat <- yMat
  }


  ## check that ts.diff can be flattened
  ##Error in "/.difftime"((dist.yz + dist.zy), ts.diff) : 
  ##        second argument of / cannot be a difftime object
  ts.diff <- unclass(ts.diff)
  
 
  ## Distance from y[i] to z[i]
  dist.yz <- double(n)
  ## Distance from z[i] to y[i+1]
  dist.zy <- double(n)

  ## Initialize the ys to be the xs.
  ys <- xs
  ## Initialize the zs to lie in between the ys
  zs <- (xs[-1,]+xs[-(n+1),])/2

  ## Initialize distances
  dist.yz <- gc.dist(ys[-(n+1),],zs)
  dist.zy <- gc.dist(zs,ys[-1,])

  ## components of the log likelihood
  ## likelihood associated with dist from y_i to x_i
  lik.xy <- dloc(xs,ys,sigma.l)

  ## likelihood associated with dist from y_i to y_(i+1)
  ## there is one for each z
  speeds <- (dist.yz+dist.zy)/ts.diff
  lik.z <- dspeed(speeds)

  for(k in 1:iters) {
   cat(k,"\n")
    ## Update ys
    ## We propose a new y[i,], compute distances and components
    ## of likelihood for the new point, and then use Metropolis
    ## Hastings rule to accept or reject proposed point
    for(i in 2:n) {

      ## propose new yi
      ## propose from some bivariate normal
      
      y.i <- rbinorm(ys[i,],sigma.y)

      ## Distance to previous z
      dist.zyi <- gc.dist(zs[i-1,,drop=F],y.i)
      ## Distance to next z
      dist.yiz <- gc.dist(y.i,zs[i,,drop=F])

      ## Distance to previous y
      dist.prev <- dist.yz[i-1]+dist.zyi
      ## Distance to next y
      dist.next <- dist.yiz+dist.zy[i]

      ## New components of likelihood
      lik.xyi <- dloc(xs[i,,drop=F],y.i,sigma.l)
      lik.z1 <- dspeed(dist.prev/ts.diff[i-1])
      lik.z2 <- dspeed(dist.next/ts.diff[i])


      ## compute difference in log posterior
      p.diff <- (lik.xyi+lik.z1+lik.z2)-(lik.xy[i]+lik.z[i-1]+lik.z[i])


      ## accept or reject
      if(p.diff > log(runif(1))) {
        ## Replace current y
        ys[i,] <- y.i
        ## Update components of likelihood
        lik.xy[i] <- lik.xyi
        lik.z[i-1] <- lik.z1
        lik.z[i] <- lik.z2
        ## Update the distances
        dist.zy[i-1] <- dist.zyi
        dist.yz[i] <- dist.yiz
      }
    }

   
    ## Update zs
    ## We propose a new z[i,], compute distances and the component
    ## of likelihood for the new point, and then use Metropolis
    ## hastings rule to accept or reject proposed point
    for(i in 1:n) {
     
      ## propose new zi
      ## propose from some bivariate normal
      z.i <- rbinorm(zs[i,],sigma.z)


      ## Distance to previous y
      dist.yzi <- gc.dist(ys[i,,drop=F],z.i)
      ## Distance to next y
      dist.ziy <- gc.dist(z.i,ys[i+1,,drop=F])

      ## Distance from previous y to next y
      dist.zi <- dist.yzi+dist.ziy
      
      ## New components of likelihood
      lik.zi <- dspeed(dist.zi/ts.diff[i])

      ## compute difference in log posterior
      p.diff <- lik.zi-lik.z[i]
      ## accept or reject
      if(p.diff > log(runif(1))) {
        ## Replace current y
        zs[i,] <- z.i
        ## Update components of likelihood
        lik.z[i] <- lik.zi
        ## Update the distances
        dist.yz[i] <- dist.yzi
        dist.zy[i] <- dist.ziy
      }
    }

    
    ## Bin into the grids
    ## For each y histogram into the y grid
    ## For each z histogram into the z grid
    #cat("k: ",k," dim(zs) ",dim(zs)," nn ",nn,"\n")
    for (nn in 2:(nrow(ys)-1)) {

      yMat <- increMat(yMat,c(ys[nn,2],ys[nn,1]),quiet=T)
      if (nn < nrow(zs)) zMat <- increMat(zMat,c(zs[nn,2],zs[nn,1]),quiet=T)

    }
    
    #if (k %% kmod == 0) {
     #opar <- par(mfrow=c(2,1))
      image(yMat)
      points(xs[,2],xs[,1])
      lines(xs[,2],xs[,1])
      lines(ys[,2],ys[,1],col="green")
      lines(zs[,2],zs[,1],col="blue")
     # image(zMat)
     # points(xs[,2],xs[,1])
     # lines(xs[,2],xs[,1])
     # lines(ys[,2],ys[,1],col="green")
     # lines(zs[,2],zs[,1],col="blue")
    #par(opar)
    #}
  }
  
  list(y=yMat,z=zMat,py=ys,pz=zs)
}
