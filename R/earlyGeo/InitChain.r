
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

