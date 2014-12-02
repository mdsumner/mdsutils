
## The start and end locations will be determined from where the tag
## was released/recaptured.  The end value is really only going to be
## of value if the tag is still running when its recaptured - else too
## much time will pass from last estimate to recapture point and the
## animal could travel anywhere in that time.



start <- c(start.lat,start.lon)
end <- c(end.lat,end.lon)

npars <- 3
nsegs <- length(unique(data$segment))

## We also need to supply covariance matrices for the proposal
## distribution (one for each segment/twilight).  These are more
## difficult to determine. The first go is pure guess.

cov.ps <- array(0,c(npars,npars,nsegs))
for(i in 1:nsegs) cov.ps[,,i] <- diag(c(0.5,0.5,0.05),npars,npars)


## Three trial runs. The main aim of the trial runs is to try to
## improve the covariance matrices that define the proposal
## distribution.  We run the chain, look at how it has progressed and
## if it is not too bad scaled down the estimated covariances for use
## with the next run.


ps <- ps.mode
for(k in 1:3) {
  summ <- sMat(dimYX=c(100,100),xlim=range(masks$x),ylim=range(masks$y))
 ## Run the chain and plot
  ch <-  metropolis(nlogpost,masks,ps,cov.ps,start,end,summ,iter=200,step=10)
  matplot(scale(ch$p,center=T,scale=F),type="l")
  save(ch,file=paste(k,"ch.Rdata"))
  ## Update ps and cov.ps for next run
  for(i in 1:nsegs) cov.ps[,,i] <- (0.3^2)*cov(ch$p[,(i-1)*npars+1:npars])
  ps <- ch$last
  #save(ch,file = paste(k,"chain.Rdata",sep = "_"))
  #save(ch$arr,file = paste(k,"arr.Rdata",sep = "_"))	
}

summ <- sMat(dimYX=c(100,100),xlim=range(masks$x),ylim=range(masks$y))
## Final run
ch <-  metropolis(nlogpost,masks,ps,cov.ps,start,end,summ,iter=1000,step=10)
ps <- ch$last
matplot(scale(ch$p,center=T,scale=F),type="l")
for(i in 1:nsegs) cov.ps[,,i] <- 0.3*cov(ch$p[,(i-1)*npars+1:npars])

summ <- sMat(dimYX=c(100,100),xlim=range(masks$x),ylim=range(masks$y))
## Final run
ch <-  metropolis(nlogpost,masks,ps,cov.ps,start,end,summ,iter=1000,step=10)
ps <- ch$last
matplot(scale(ch$p,center=T,scale=F),type="l")
for(i in 1:nsegs) cov.ps[,,i] <- 0.3*cov(ch$p[,(i-1)*npars+1:npars])

summ <- sMat(dimYX=c(100,100),xlim=range(masks$x),ylim=range(masks$y))
## Final run
for (n in 1:50) {
ch <-  metropolis(nlogpost,masks,ps,cov.ps,start,end,summ,iter=1000,step=10)
ps <- ch$last
matplot(scale(ch$p,center=T,scale=F),type="l")
for(i in 1:nsegs) cov.ps[,,i] <- 0.3*cov(ch$p[,(i-1)*npars+1:npars])

}


