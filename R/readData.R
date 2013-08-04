#' ~~function to do ... ~~
#' 
#' ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' ~~ If necessary, more details than the description above ~~
#' 
#' @param x ~~Describe \code{x} here~~
#' @param xlim ~~Describe \code{xlim} here~~
#' @param ylim ~~Describe \code{ylim} here~~
#' @param obj ~~Describe \code{obj} here~~
#' @return ~Describe the value returned If it is a LIST, use
#' 
#' ...
#' @returnItem comp1 Description of 'comp1'
#' @returnItem comp2 Description of 'comp2'
#' @note ~~further notes~~
#' @author ~~who you are~~
#' @seealso ~~objects to See Also as \code{\link{help}}, ~~~
#' @references ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function(x, xlim = c(140, 170), ylim = c(-70, -30), obj = NULL) {
#' 	if (length(x) >1) warning("only one file may be read at a time: only first element of x will be used")
#' 	if (is.null(obj)) stop("\"obj\" missing: must supply a data description object\n see mkMCSST() for example")
#' 	
#' 	cGrid <- clipGrid(obj$grid, xlim, ylim, obj$upside.down)
#' 	res <- list()
#' 	if (attr(cGrid, "case") == "case3") {p4 <- paste(as.character(CRSargs(obj$p4)), "+over")} else {p4 <- as.character(CRSargs(obj$p4))}
#' 
#' 	for (gr in 1:length(cGrid)) {
#' 		grid.region <- cGrid[[gr]]
#' 		offset <- grid.region$offset
#' 		region.dim <- grid.region$region.dim
#' 	
#' 		bnames <- obj$bands
#' 		dnames <- paste(obj$sdsTemplate[1], x[1], obj$sdsTemplate[2], obj$bandNumbers, sep = "")
#' 	
#' 		xout <- readGDAL(dnames[1], offset = offset, region.dim = region.dim) 
#' 		if (length(dnames) > 1) {
#' 			for (i in 2:length(dnames)) {
#' 				xout[[i]] <- readGDAL(dnames[i], offset = offset, region.dim = region.dim)$band1
#' 			}	
#' 		}
#' 		res[[gr]] <- SpatialGridDataFrame(grid.region$grid, xout@data, proj4string = CRS(p4))
#' 	}
#' 	
#' 	
#' 	if (length(res) > 1) {
#' 		c1 <- coordinates(res[[1]])
#' 		c2 <- coordinates(res[[2]])
#' 		
#' 		## what about when p4 is NA ??
#' 		if (attr(cGrid, "case") == "case2")  {c2[,1] <- c2[,1] + 360; p4 <- paste(as.character(CRSargs(obj$p4)), "+over")}
#' 
#' 		if (attr(cGrid, "case") == "case4")  {c1[,1] <- c1[,1] - 360; p4 <- gsub("\+over", "", as.character(CRSargs(obj$p4)))}
#' 
#' 		#if (attr(cGrid, "case") == "case2") grid <- combineGrid(cGrid[[1]]$grid, cGrid[[2]]$grid)
#' 		#if (attr(cGrid, "case") == "case4") grid <- combineGrid(cGrid[[2]]$grid, cGrid[[1]]$grid)
#' 		#xout <- SpatialGridDataFrame(grid, rbind(res[[1]]@data, res[[2]]@data))
#' 		xout <- SpatialPixelsDataFrame(rbind(c1,c2), rbind(res[[1]]@data, res[[2]]@data), proj4string = CRS(p4))
#' 		fullgrid(xout) <- TRUE
#' 
#' 	
#' 	} else {xout <- res[[1]]}
#' 	
#' 	names(xout) <- bnames
#' 	#ind <- match(c("mcsst", "interpolated"), names(x))
#' 	ind <- match(obj$scaledData, names(xout))
#' 	if (any(!is.na(ind))) {
#' 	for (nm in ind) {
#' 		xout[[nm]] <- obj$sclEquation(xout[[nm]])
#' 		xout[[nm]][xout[[nm]] < obj$Validrange[1] | xout[[nm]] > obj$Validrange[2]] <- NA
#' 		}
#' 	}
#' 	## recreate with grid topology and projection string
#' 	if (!is.null(obj$flipFunction)) xout <- obj$flipFunction(xout)
#' 	#SpatialGridDataFrame(grid, xout@data)
#' 	return(xout)
#' 	
#' 	
#'   }
#' 
`readData` <-
function(x, xlim = c(140, 170), ylim = c(-70, -30), obj = NULL) {
	if (length(x) >1) warning("only one file may be read at a time: only first element of x will be used")
	if (is.null(obj)) stop("\"obj\" missing: must supply a data description object\n see mkMCSST() for example")
	
	cGrid <- clipGrid(obj$grid, xlim, ylim, obj$upside.down)
	res <- list()
	case <- attr(cGrid, "case")
	if (attr(cGrid, "case") == "case3") {p4 <- paste(as.character(CRSargs(obj$p4)), "+over")} else {p4 <- as.character(CRSargs(obj$p4))}

	p4 <- as.character(obj$p4@projargs)
		if (!(case == "case1") & length(grep("longlat", p4)) == 0 ) {
			 stop("world wrap clipping only supported for longlat data")
	}

	for (gr in 1:length(cGrid)) {
		grid.region <- cGrid[[gr]]
		offset <- grid.region$offset
		region.dim <- grid.region$region.dim
	
		bnames <- obj$bands
		dnames <- paste(obj$sdsTemplate[1], x[1], obj$sdsTemplate[2], obj$bandNumbers, sep = "")
	
		xout <- readGDAL(dnames[1], offset = offset, region.dim = region.dim) 
		if (length(dnames) > 1) {
			for (i in 2:length(dnames)) {
				xout[[i]] <- readGDAL(dnames[i], offset = offset, region.dim = region.dim)$band1
			}	
		}
		res[[gr]] <- SpatialGridDataFrame(grid.region$grid, xout@data, proj4string = CRS(p4))
	}
	
	
	if (length(res) > 1) {
		c1 <- coordinates(res[[1]])
		c2 <- coordinates(res[[2]])
		
		## what about when p4 is NA ??
		if (attr(cGrid, "case") == "case2")  {c2[,1] <- c2[,1] + 360; p4 <- paste(as.character(CRSargs(obj$p4)), "+over")}

		if (attr(cGrid, "case") == "case4")  {c1[,1] <- c1[,1] - 360; p4 <- gsub("\\+over", "", as.character(CRSargs(obj$p4)))}

		#if (attr(cGrid, "case") == "case2") grid <- combineGrid(cGrid[[1]]$grid, cGrid[[2]]$grid)
		#if (attr(cGrid, "case") == "case4") grid <- combineGrid(cGrid[[2]]$grid, cGrid[[1]]$grid)
		#xout <- SpatialGridDataFrame(grid, rbind(res[[1]]@data, res[[2]]@data))
		xout <- SpatialPixelsDataFrame(rbind(c1,c2), rbind(res[[1]]@data, res[[2]]@data), proj4string = CRS(p4))
		fullgrid(xout) <- TRUE

	
	} else {xout <- res[[1]]}
	
	names(xout) <- bnames
	#ind <- match(c("mcsst", "interpolated"), names(x))
	ind <- match(obj$scaledData, names(xout))
	if (any(!is.na(ind))) {
	for (nm in ind) {
		xout[[nm]] <- obj$sclEquation(xout[[nm]])
		xout[[nm]][xout[[nm]] < obj$Validrange[1] | xout[[nm]] > obj$Validrange[2]] <- NA
		}
	}
	## recreate with grid topology and projection string
	if (!is.null(obj$flipFunction)) xout <- obj$flipFunction(xout)
	#SpatialGridDataFrame(grid, xout@data)
	return(xout)
	
	
}

