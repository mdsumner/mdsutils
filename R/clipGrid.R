#' ~~function to do ... ~~
#' 
#' ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' There are four cases to be considered, horizontal lines indicate an example
#' are of interest from the longlat world:
#' 
#' \preformatted{ normal convention, no wrap
#' 
#' |;;;;;;;;;;;;;;|;;;;;;;;;;;;;;| |;;;;;;;;;;----|----;;;;;;;;;;|
#' -180|;;;;;;;;;;----0----;;;;;;;;;;|180 |;;;;;;;;;;----|----;;;;;;;;;;|
#' |;;;;;;;;;;;;;;|;;;;;;;;;;;;;;|
#' 
#' normal convention, dateline wrap
#' 
#' |;;;;;;;;;;;;;;|;;;;;;;;;;;;;;| |----;;;;;;;;;;|;;;;;;;;;;----|
#' -180|----;;;;;;;;;;0;;;;;;;;;;----|180 |----;;;;;;;;;;|;;;;;;;;;;----|
#' |;;;;;;;;;;;;;;|;;;;;;;;;;;;;;|
#' 
#' pacific convention, no wrap
#' 
#' |;;;;;;;;;;;;;;|;;;;;;;;;;;;;;| |;;;;;;;;;;----|----;;;;;;;;;;|
#' 0|;;;;;;;;;;---180---;;;;;;;;;;|360 |;;;;;;;;;;----|----;;;;;;;;;;|
#' |;;;;;;;;;;;;;;|;;;;;;;;;;;;;;|
#' 
#' pacific convention, greenwich wrap
#' 
#' |;;;;;;;;;;;;;;|;;;;;;;;;;;;;;| |----;;;;;;;;;;|;;;;;;;;;;----|
#' 0|----;;;;;;;;;180;;;;;;;;;----|360 |----;;;;;;;;;;|;;;;;;;;;;----|
#' |;;;;;;;;;;;;;;|;;;;;;;;;;;;;;| }
#' 
#' @param x ~~Describe \code{x} here~~
#' @param xlim ~~Describe \code{xlim} here~~
#' @param ylim ~~Describe \code{ylim} here~~
#' @param upside.down ~~Describe \code{upside.down} here~~
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
#' function(x, xlim, ylim, upside.down = FALSE) {
#' 	
#' 	bb <- x@cellcentre.offset - x@cellsize/2
#' 	bb <- cbind(bb, bb + (x@cells.dim)*x@cellsize)
#' 	colnames(bb) <- c("min", "max")
#' 
#' 	## what situation do we have?
#' 	if (xlim[1] >= bb[1,1] & xlim[2] <= bb[1,2]) case <- "case1"
#' 	if (xlim[2] > bb[1,2]) case <- "case2"
#' 	if (xlim[1] > bb[1,2] & xlim[2] > bb[1,2]) case <- "case3"
#' 	if (xlim[1] < bb[1,1]) case <- "case4"
#' 	if (xlim[1] < bb[1,1] & xlim[2] < bb[1,1]) case <- "case5"
#' 	
#' 	if (case %in% c("case3", "case5")) {print(bb);stop("xlim doesn't match data bounds ", paste(xlim, collapse = ","))}
#' 	print(case)
#' 	
#' 	###################################################################
#' 	xy <- coordinatevalues(x)
#' 	names(xy) <- c("x", "y")  
#' 	
#' 	xsub <- xy$x >= xlim[1] & xy$x <= xlim[2]
#' 	ysub <- xy$y >= ylim[1] & xy$y <= ylim[2]
#' 	xy$x <- xy$x[xsub]
#' 	xy$y <- xy$y[ysub]
#' 	cc.offset <- c(min(xy$x), min(xy$y))
#' 	csize <- c(diff(xy$x[1:2]), diff(rev(xy$y)[1:2]))
#' 	cdim <- c(length(xy$x), length(xy$y))
#' 	offset <- c(min(which(ysub)), min(which(xsub))) - 1
#' 	
#' 	if (upside.down) offset[1] <- x@cells.dim[2] - offset[1] - cdim[2] 
#' 	region.dim <- c(length(xy$y), length(xy$x))
#' 	
#' 	g1 <- list(grid = GridTopology(cc.offset, csize, cdim), offset = offset, region.dim = region.dim)
#' 	##################################################################################################
#' 	if (case %in% c("case1", "case3")) {res <- list(g1); attr(res, "case") <- case;return(res)}
#' 	
#' 	if (case == "case2")  {
#' 		xlim <- c(-180, xlim[2] - 360)
#' 		###################################################################
#' 		xy <- coordinatevalues(x)
#' 		names(xy) <- c("x", "y")  
#' 	
#' 		xsub <- xy$x >= xlim[1] & xy$x <= xlim[2]
#' 		ysub <- xy$y >= ylim[1] & xy$y <= ylim[2]
#' 		xy$x <- xy$x[xsub]
#' 		xy$y <- xy$y[ysub]
#' 		cc.offset <- c(min(xy$x), min(xy$y))
#' 		csize <- c(diff(xy$x[1:2]), diff(rev(xy$y)[1:2]))
#' 		cdim <- c(length(xy$x), length(xy$y))
#' 		offset <- c(min(which(ysub)), min(which(xsub))) - 1
#' 		
#' 		if (upside.down) offset[1] <- x@cells.dim[2] - offset[1] - cdim[2] 
#' 		region.dim <- c(length(xy$y), length(xy$x))
#' 		
#' 		g2 <- list(grid = GridTopology(cc.offset, csize, cdim), offset = offset, region.dim = region.dim)
#' 		##################################################################################################
#' 		res <- list(g1, g2)
#' 		
#' 	}
#' 	
#' 	if (case == "case4") {
#' 		xlim <- c(180, xlim[2] + 360)
#' 		g2 <- g1
#' 		###################################################################
#' 		xy <- coordinatevalues(x)
#' 		names(xy) <- c("x", "y")  
#' 	
#' 		xsub <- xy$x >= xlim[1] & xy$x <= xlim[2]
#' 		ysub <- xy$y >= ylim[1] & xy$y <= ylim[2]
#' 		xy$x <- xy$x[xsub]
#' 		xy$y <- xy$y[ysub]
#' 		cc.offset <- c(min(xy$x), min(xy$y))
#' 		csize <- c(diff(xy$x[1:2]), diff(rev(xy$y)[1:2]))
#' 		cdim <- c(length(xy$x), length(xy$y))
#' 		offset <- c(min(which(ysub)), min(which(xsub))) - 1
#' 		
#' 		if (upside.down) offset[1] <- x@cells.dim[2] - offset[1] - cdim[2] 
#' 		region.dim <- c(length(xy$y), length(xy$x))
#' 				
#' 		g1 <- list(grid = GridTopology(cc.offset, csize, cdim), offset = offset, region.dim = region.dim)
#' 		##################################################################################################
#' 		res <- list(g1, g2)
#' 	
#' 	}
#' 	attr(res, "case") <- case
#' 	return(res)
#' 	stop("never get here: check your xlim versus the bounds of the dataset")
#' 	
#' 	
#'   }
#' 
`clipGrid` <-
function(x, xlim, ylim, upside.down = FALSE) {
	
	bb <- x@cellcentre.offset - x@cellsize/2
	bb <- cbind(bb, bb + (x@cells.dim)*x@cellsize)
	colnames(bb) <- c("min", "max")
	rownames(bb) <- c("x", "y")
	## what situation do we have?
	if (xlim[1] >= bb[1,1] & xlim[2] <= bb[1,2]) case <- "case1"
	if (xlim[2] > bb[1,2]) case <- "case2"
	if (xlim[1] > bb[1,2] & xlim[2] > bb[1,2]) case <- "case3"
	if (xlim[1] < bb[1,1]) case <- "case4"
	if (xlim[1] < bb[1,1] & xlim[2] < bb[1,1]) case <- "case5"
	
	if (case %in% c("case3", "case5")) 
		{print(bb);stop("xlim doesn't match data bounds ", paste(xlim, collapse = ","))}
	#if (ylim[1] < bb[2,1] | ylim[1] > bb[2,2] | ylim[2] < bb[2,1] | ylim[2] > bb[2,2]) 
	#	{print(bb);stop("ylim doesn't match data bounds ", paste(ylim, collapse = ","))}
	if (ylim[1] < bb[2,1] | ylim[1] > bb[2,2] | ylim[2] < bb[2,1] | ylim[2] > bb[2,2]) 
		{print(bb);stop("ylim doesn't match data bounds ", paste(ylim, collapse = ","))}
	print(case)
	
	###################################################################
	xy <- coordinatevalues(x)
	names(xy) <- c("x", "y")  
	
	xsub <- xy$x >= xlim[1] & xy$x <= xlim[2]
	ysub <- xy$y >= ylim[1] & xy$y <= ylim[2]
	xy$x <- xy$x[xsub]
	xy$y <- xy$y[ysub]
	cc.offset <- c(min(xy$x), min(xy$y))
	csize <- c(diff(xy$x[1:2]), diff(rev(xy$y)[1:2]))
	cdim <- c(length(xy$x), length(xy$y))
	offset <- c(min(which(ysub)), min(which(xsub))) - 1
	
	if (upside.down) offset[1] <- x@cells.dim[2] - offset[1] - cdim[2] 
	region.dim <- c(length(xy$y), length(xy$x))
	
	g1 <- list(grid = GridTopology(cc.offset, csize, cdim), offset = offset, region.dim = region.dim)
	##################################################################################################
	if (case %in% c("case1", "case3")) {res <- list(g1); attr(res, "case") <- case;return(res)}
	
	if (case == "case2")  {
		xlim <- c(-180, xlim[2] - 360)
		###################################################################
		xy <- coordinatevalues(x)
		names(xy) <- c("x", "y")  
	
		xsub <- xy$x >= xlim[1] & xy$x <= xlim[2]
		ysub <- xy$y >= ylim[1] & xy$y <= ylim[2]
		xy$x <- xy$x[xsub]
		xy$y <- xy$y[ysub]
		cc.offset <- c(min(xy$x), min(xy$y))
		csize <- c(diff(xy$x[1:2]), diff(rev(xy$y)[1:2]))
		cdim <- c(length(xy$x), length(xy$y))
		offset <- c(min(which(ysub)), min(which(xsub))) - 1
		
		if (upside.down) offset[1] <- x@cells.dim[2] - offset[1] - cdim[2] 
		region.dim <- c(length(xy$y), length(xy$x))
		
		g2 <- list(grid = GridTopology(cc.offset, csize, cdim), offset = offset, region.dim = region.dim)
		##################################################################################################
		res <- list(g1, g2)
		
	}
	
	if (case == "case4") {
		xlim <- c(180, xlim[2] + 360)
		g2 <- g1
		###################################################################
		xy <- coordinatevalues(x)
		names(xy) <- c("x", "y")  
	
		xsub <- xy$x >= xlim[1] & xy$x <= xlim[2]
		ysub <- xy$y >= ylim[1] & xy$y <= ylim[2]
		xy$x <- xy$x[xsub]
		xy$y <- xy$y[ysub]
		cc.offset <- c(min(xy$x), min(xy$y))
		csize <- c(diff(xy$x[1:2]), diff(rev(xy$y)[1:2]))
		cdim <- c(length(xy$x), length(xy$y))
		offset <- c(min(which(ysub)), min(which(xsub))) - 1
		
		if (upside.down) offset[1] <- x@cells.dim[2] - offset[1] - cdim[2] 
		region.dim <- c(length(xy$y), length(xy$x))
				
		g1 <- list(grid = GridTopology(cc.offset, csize, cdim), offset = offset, region.dim = region.dim)
		##################################################################################################
		res <- list(g1, g2)
	
	}
	attr(res, "case") <- case
	return(res)
	stop("never get here: check your xlim versus the bounds of the dataset")
	
	
}

