setOldClass("zoo")

setClass("trip1", contains = "zoo")


setMethod("[", "trip",
          function(x, i, j, ... , drop = TRUE) {
              missing.i = missing(i)
              missing.j = missing(j)
              nargs = nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
              if (missing.i && missing.j) {
                  i = TRUE
                  j = TRUE
              } else if (missing.j && !missing.i) {
                  if (nargs == 2) {
                      j = i
                      i = TRUE
                  } else {
                      j = TRUE


                      tor <- getTORnames(x)
                      IDs <- x[[tor[2]]]
                      if (i > length(unique(IDs))) stop()
return(                      x[IDs == unique(IDs)[i],])


                  }
              } else if (missing.i && !missing.j)
                  i = TRUE
              if (is.matrix(i))
                  stop("matrix argument not supported in SpatialPointsDataFrame selection")
              if (any(is.na(i)))
                  stop("NAs not permitted in row index")

              spdf <- as(x, "SpatialPointsDataFrame")[i, j, ..., drop = drop]
              tor <- getTORnames(x)
              if ( is.factor(spdf[[tor[2]]])) spdf[[tor[2]]] <- factor(spdf[[tor[2]]])
              if (any(is.na(match(tor, names(spdf))))) {
                  cat("trip-defining Date or ID columns dropped, reverting to SpatialPointsDataFrame\n\n")

                  return(spdf)
              } else if (any(tapply(spdf[[tor[1]]], spdf[[tor[2]]], length) < 3)){
                  cat("subset loses too many locations, reverting to SpatialPointsDataFrame\n\n")
                  return(spdf)
              } else {
                  return(trip(spdf, tor))
              }
          }
          )
