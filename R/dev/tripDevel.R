
#setClass("trip", contains = c("TimeOrderedRecords", "SpatialPointsDataFrame"))

setClass("trip", contains = "SpatialPointsDataFrame")

validtordata <- function(object) {
	if (!is(object@data, "data.frame"))
		stop("only data frames supported for data slots")
		
       coords <- coordinates(object)
       if (dim(coords)[2] < 4) stop("coordinates must contain at least four columns")
	
	## assume 3rd and 4th columns for now
	tid <- as.data.frame(coords[,3:4])
	
	#tid <- as.data.frame(object@data[ , object@TOR.columns])
	if (length(tid) == 0)
		stop("timeIDs cannot have zero length")
	if (nrow(tid) < 1)
		stop("no timeIDs set: too few rows")
	if (ncol(tid) < 2)
		stop("no timeIDs set: too few columns")
		
	if (any(duplicated(as.data.frame(object)))) stop("duplicated records within data")
    if (any(duplicated(as(object, "data.frame")))) stop("duplicated records within data")

  time <- tid[,1]
  
  
  ## force this for now
  class(time) <- c("POSIXt", "POSIXct")
  
  
  id <- tid[, 2]
  TORlevs <- levels(factor(id))

  
  if (!is(time, "POSIXt")) stop("trip only handles dates and times as POSIXt objects")


  bad <- c(is.na(time), !is.finite(time), is.na(id), !is.finite(id))
  if (any(bad)) return("time and/or id data contains missing or non finite values")
  d <- unlist(tapply(time, id, diff))
  if (any(d < 0)) return("date-times not in order within id")
  if (any(d == 0)) return("date-times contain duplicates within id")
  short <- which(unlist(tapply(time, id, length)) < 3)
     ## maybe trip enforces this
  if (length(short)>0) {
    mess <- "\n  less than 3 locations for ids:\n"
    mess <- paste(mess, paste(TORlevs[short],  collapse = ","), sep = "")
    return(mess)
  }
  return(TRUE)
}


setValidity("trip", validtordata)
as.data.frame.trip <- function(x, ...) as.data.frame(as(x, "SpatialPointsDataFrame"), ...)



if (!isGeneric("trip"))
	setGeneric("trip", function(obj, TORnames)
		standardGeneric("trip"))


trip <- function(obj, TORnames) {
		## only spdf for now
	if ( !is(obj, "SpatialPointsDataFrame") ) {
		stop("trip only supports SpatialPointsDataFrame")	#ANY?
	}	
	if (is.factor(obj[[TORnames[2]]])) obj[[TORnames[2]]] <- factor(obj[[TORnames[2]]])
	new("trip", obj, TimeOrderedRecords(TORnames))

}

## removed as this was causing recursion in 2.8.0
#setMethod("trip", signature(obj = "SpatialPointsDataFrame", TORnames = "ANY"), trip)

setMethod("trip", signature(obj = "ANY", TORnames = "TimeOrderedRecords"), 
	function(obj, TORnames) {
		new("trip", obj, TORnames)

	})
	
	setMethod("trip", signature(obj = "trip", TORnames = "TimeOrderedRecords"), 
		function(obj, TORnames) {
			new("trip", as(obj, "SpatialPointsDataFrame"), TORnames)
	
	})
	
setMethod("trip", signature(obj = "trip", TORnames = "ANY"), 
	function(obj, TORnames) {
	##trip.default(as(obj, "SpatialPointsDataFrame"), TORnames)
	trip(as(obj, "SpatialPointsDataFrame"), TORnames)
})

#setMethod("trip", signature(obj = "ANY", col.nms = "TimeOrderedRecords"), 
#	function(obj, col.nms) {
#	 trip(obj, col.nms)
#	 })
	 
#setMethod("trip", signature(obj = "SpatialPointsDataFrame", col.nms = "character"),
	#function(obj, col.nms) new("trip", obj, TimeOrderedRecords(col.nms))
#)


## works already:
## coordinates - not replace
## print
## show
## plot
## summary
## "[" - not replace

## doesn't work already
## dim
## as.data.frame
## names - and replace
## points
## text
## subset
#"[[", "$"
## split

## S3 versions
dim.trip <- function(x) dim(as(x, "SpatialPointsDataFrame"))
#as.data.frame.trip <- function(x, ...) as.data.frame(as(x, "SpatialPointsDataFrame"), ...)
#"[[.trip" =  function(x, ...) as(x, "SpatialPointsDataFrame")[[...]]


##  not needed -  by global "Spatial" method
#setMethod("[[", c("trip", "ANY", "missing"), function(x, i, j, ...)
#		x@data[[i]]
#	)	


	
setReplaceMethod("[[", c("trip", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
	
		tor <- getTORnames(x)
		x <- as(x, "SpatialPointsDataFrame")
		x[[i]] <- value	
		trip(x, tor)
	
	}
)

	##  not needed -  by global "Spatial" method
	#setMethod("$", c("trip", "character"), 
	#	function(x, name) x@data[[name]]
	#)
	
	## needed to ensure validity of returned object
	setReplaceMethod("$", c("trip", "character", "ANY"), 
		function(x, name, value) { 
			tor <- getTORnames(x)
			x <- as(x, "SpatialPointsDataFrame")
			
			x[[name]] <- value
				
			trip(x, tor)
		}
	)
	
	
#"[[<-.trip" =  function(x, i, j, value) {
#
#	tor <- getTORnames(x)
#	x <- as(x, "SpatialPointsDataFrame")
#	x[[i]] <- value	
#	trip(x, tor)
#}

#"$.trip" = function(x, name) x@data[[name]]

#"$<-.trip" = function(x, i, value) {
#	tor <- getTORnames(x)
#	x <- as(x, "SpatialPointsDataFrame")

#	x[[i]] <- value
	
#	trip(x, tor)
	
#	}
	
##setMethod("names", "trip", function(x) names(as(x, "SpatialPointsDataFrame")))
##setMethod("names", "trip", function(x) names(x@data))
names.trip <- function(x) names(as(x, "SpatialPointsDataFrame"))
"names<-.trip" <- function(x, value) { names(x@data) = value;  x@TOR.columns = value; x }


setMethod("points", "trip", function(x, ...) points(as(x, "SpatialPointsDataFrame"), ...))
setMethod("text", "trip", function(x, ...) text(as(x, "SpatialPointsDataFrame"), ...))

#setMethod("split", "SpatialPointsDataFrame", split.data.frame)


#subset.trip <- function(x, subset, select, drop = FALSE, ...) {
subset.trip <- function(x,  ...) {
			#spdf <- subset(as(x, "SpatialPointsDataFrame"), subset, select, drop = drop, ...)
			spdf <- subset(as(x, "SpatialPointsDataFrame"), ...)
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

setMethod("[", "trip",
	function(x, i, j, ... , drop = TRUE) {
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

setMethod("subset", "trip", subset.trip)

#### summary and print
summary.tordata <- function(object, ...) {


  
  obj <- list(spdf = summary(as(object, "SpatialPointsDataFrame")))

   ## method or not here?
  time <- object[[object@TOR.columns[1]]]
  ids <- object[[object@TOR.columns[2]]]

  tmins <- tapply(time, ids, min) + ISOdatetime(1970, 1, 1, 0, 0,0, tz = "GMT")
  tmaxs <- tapply(time, ids, max) + ISOdatetime(1970, 1, 1, 0, 0,0, tz = "GMT")
  nlocs <- tapply(time, ids, length)
  obj[["class"]] <- class(object)
  obj[["tmins"]] <- tmins
  obj[["tmaxs"]] <- tmaxs
  obj[["tripID"]] <- levels(factor(ids))
  obj[["nRecords"]] <- nlocs
  obj[["TORnames"]] <- getTORnames(object)
  obj[["tripDuration"]] <- tapply(time, ids, function(x) {x <- format(diff(range(x)))})

  class(obj) <- "summary.tordata"
  #invisible(obj)
  obj
}







print.summary.tordata <- function(x, ...) {
  dsumm <- data.frame(tripID = x$tripID, No.Records = x$nRecords, startTime = x$tmins, endTime = x$tmaxs, tripDuration = x$tripDuration)

  names(dsumm)[1] <- paste(names(dsumm)[1], " (\"", x[["TORnames"]][2], "\")", sep = "")
  names(dsumm)[3] <- paste(names(dsumm)[3], " (\"", x[["TORnames"]][1], "\")", sep = "")
   names(dsumm)[4] <- paste(names(dsumm)[4], " (\"", x[["TORnames"]][1], "\")", sep = "")
  rownames(dsumm) <- 1:nrow(dsumm)
  #dsumm <- as.data.frame(lapply(dsumm, as.character))
  cat(paste("\nObject of class ", x[["class"]], "\n", sep = ""))
  print(format(dsumm, ...))
  cat(paste("\nDerived from Spatial data:\n\n", sep = ""))
  print(x$spdf)
  cat("\n")
}

print.trip <- function(x, ...) {
  xs <- summary(x)
  dsumm <- data.frame(tripID = xs$tripID, No.Records = xs$nRecords, startTime = xs$tmins, endTime = xs$tmaxs, tripDuration = xs$tripDuration)
  names(dsumm)[1] <- paste(names(dsumm)[1], " (\"", xs[["TORnames"]][2], "\")", sep = "")
  names(dsumm)[3] <- paste(names(dsumm)[3], " (\"", xs[["TORnames"]][1], "\")", sep = "")
   names(dsumm)[4] <- paste(names(dsumm)[4], " (\"", xs[["TORnames"]][1], "\")", sep = "")
  rownames(dsumm) <- 1:nrow(dsumm)
   #dsumm <- as.data.frame(lapply(dsumm, as.character))
  cat(paste("\nObject of class ", xs[["class"]], "\n", sep = ""))
  print(format(dsumm, ...))
  cat("\n")
  nms <- names(x)
  #names(nms) <- names(x)
  #nms[[xs[["TORnames"]][1]]] <- paste(nms[[xs[["TORnames"]][1]]], "*trip DateTime*")
  #nms[[xs[["TORnames"]][2]]] <- paste(nms[[xs[["TORnames"]][2]]], "#trip ID#")
  clss <- unlist(lapply(as.data.frame(x@data),  function(x) class(x)[1]))
  #names(clss) <- names(x)
  #clss[[xs[["TORnames"]][1]]] <- paste(clss[[xs[["TORnames"]][1]]], "*trip DateTime*")
  #clss[[xs[["TORnames"]][2]]] <- paste(clss[[xs[["TORnames"]][2]]], "#trip ID#")
  sdf <- data.frame(data.columns = nms, data.class = clss)
  sdf[[" "]] <- rep("", nrow(sdf))
  sdf[[" "]][which(names(x) == xs[["TORnames"]][1])] <- "**trip DateTime**"
  sdf[[" "]][which(names(x) == xs[["TORnames"]][2])] <- "**trip ID**      "
  #sdf$hideme <- factor(sdf$hideme)
  #names(sdf)[3] <- ""
  row.names(sdf) <- 1:nrow(sdf)
  
  
  print(sdf)
  cat("\n")

}








setMethod("summary", "trip", summary.tordata)

setMethod("show", "trip", function(object) print.trip(object))

#setMethod("print", "trip",function(x, ...) print(as(x, "SpatialPointsDataFrame")))



#setMethod("spTransform", signature(x = "trip", "CRS"),
#	function (x, CRSobj, ...)
#	{
	    	
#	        xSP <- spTransform(as(x, "SpatialPointsDataFrame"), CRSobj, ...)
	      
#	        xDF <- x@data
#	        res <- SpatialPointsDataFrame(coords = coordinates(xSP),
#	            data = xDF, coords.nrs = numeric(0), proj4string = CRS(proj4string(xSP)))

#	        trip(res, getTORnames(x))
#	    }
#)

setMethod("lines", signature(x = "trip"), 
	function(x, ...) {
		tor <- getTORnames(x)
		lx <- split(1:nrow(x), x[[tor[2]]])
		coords <- coordinates(x)
		
			col <- hsv(seq(0, 0.5, length = length(lx)))
			for (i in 1:length(lx)) {
			        lines(coords[lx[[i]], ], col = col[i], ...)
			}	
	}
	)
setMethod("plot", signature(x = "trip", y = "missing"),
	function(x, y, ...) {
		plot(as(x, "SpatialPoints"), ...)
		
	})


#setMethod("plot", signature(x = "trip", y = "character"),
#	function(x, y, ...) {
#		plot(coordinates(x),  col = x[[y]], ...)
#})

recenter.trip <- 
function(obj) {
    proj <- is.projected(obj)
    if (is.na(proj)) {
        warning("unknown coordinate reference system: assuming longlat")
    	#projargs <- CRS("+proj=longlat")
    }
    if (!is.na(proj) & proj) 
        stop("cannot recenter projected coordinate reference system")
   projargs <- CRS(proj4string(obj))
   crds <- coordinates(obj)
   inout <- (crds[, 1] < 0)
    if (all(inout)) {
        crds[, 1] <- crds[, 1] + 360
        if (!is.na(proj)) projargs <- CRS(paste(proj4string(obj), "+over"))
    }else {
        if (any(inout)) {
            crds[, 1] <- ifelse(inout, crds[, 1] + 360, crds[, 
                1])
        if (!is.na(proj)) projargs <- CRS(paste(proj4string(obj), "+over"))
        }
    }
res <- trip(new("SpatialPointsDataFrame", SpatialPoints(crds, projargs), data = obj@data, 
        coords.nrs = obj@coords.nrs), obj@TOR.columns)
res}

setMethod("recenter", "trip", recenter.trip)
