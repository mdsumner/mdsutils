r2p <- function(x, lev) {
  ## maybe sort(unique(lev)) here? 
  valrange <- cellStats(x, range, na.rm = TRUE)
  valrange <- c(floor(valrange[1]), ceiling(valrange[2]))
  
  fac <- cut(valrange, c(valrange[1L], lev, valrange[2]))
  rc <- cbind(from = c(valrange[1L], lev), to = c(lev, valrange[2]), becomes = c(lev, valrange[2]))
  
  x2 <- reclassify(x, rc, include.lowest = TRUE)
  
  x3 <- ratify(x2)
  rat <- levels(x3)[[1]]
  rat$level <- levels(fac)
  rat$code <- rc[,"becomes"]
  levels(x3) <- rat
  rp <- rasterToPolygons(x3, dissolve = TRUE)
  rp$level <- rat$level[match(rp$layer, rat$code)]
  
  rp$from <- rc[match(rp$layer, rat$code),"from"]
  rp$to <- rc[match(rp$layer, rat$code), "to"]
  rp$layer <- NULL
  rp  
}
