#' ~~function to do ... ~~
#' 
#' ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' ~~ If necessary, more details than the description above ~~
#' 
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
#' function() {
#' 
#' 	cdim <- c(720, 360)
#' 	csize <- c(360, 180)/cdim
#' 	cc.offset <- c(0, -90) + csize/2
#' 	gt <- GridTopology(cc.offset, csize, cdim)
#' 	
#' 	sclEquation <- function(x) {
#' 		Slope <- 1.0e-3#scale_factor=1.000000e-003
#' 		Intercept <- 0#add_offset=0.000000e+000
#' 		x*Slope + Intercept 
#' 		
#' 	}
#' 	
#' 	Validrange <- c(-1.5, 1.5)
#' 	bands <- c("sealevel", "count")
#' 	scaledData <- bands[1]
#' 	sdsTemplate <- c("NETCDF:\"", "\":")
#' 	timeTemplate <- "ssh05d%Y%m%d"
#' 	flipFunction <- function(x) {
#' 		for (i in names(x)) {
#' 			x[[i]] <- as.vector(as.image.SpatialGridDataFrame(x[i])$z)
#' 		}
#' 		x
#' 		
#' 	}
#' 	p4 <- CRS("+proj=longlat +datum=WGS84")
#' 	bandNumbers <- c("sea_level", "bin_count")
#' 	list(grid = gt, sclEquation = sclEquation, Validrange = Validrange,
#' 		bands = bands, sdsTemplate = sdsTemplate, timeTemplate = timeTemplate, 
#' 		scaledData = scaledData, p4 = p4,  bandNumbers = bandNumbers,
#' 		flipFunction = flipFunction, upside.down = TRUE)
#' 	
#'   }
#' 
`mkTopex` <-
function() {

	cdim <- c(720, 360)
	csize <- c(360, 180)/cdim
	cc.offset <- c(0, -90) + csize/2
	gt <- GridTopology(cc.offset, csize, cdim)
	
	sclEquation <- function(x) {
		Slope <- 1.0e-3#scale_factor=1.000000e-003
		Intercept <- 0#add_offset=0.000000e+000
		x*Slope + Intercept 
		
	}
	
	Validrange <- c(-1.5, 1.5)
	bands <- c("sealevel", "count")
	scaledData <- bands[1]
	sdsTemplate <- c("NETCDF:\"", "\":")
	timeTemplate <- "ssh05d%Y%m%d"
	flipFunction <- function(x) {
		for (i in names(x)) {
			x[[i]] <- as.vector(as.image.SpatialGridDataFrame(x[i])$z)
		}
		x
		
	}
	p4 <- CRS("+proj=longlat +datum=WGS84")
	bandNumbers <- c("sea_level", "bin_count")
	list(grid = gt, sclEquation = sclEquation, Validrange = Validrange,
		bands = bands, sdsTemplate = sdsTemplate, timeTemplate = timeTemplate, 
		scaledData = scaledData, p4 = p4,  bandNumbers = bandNumbers,
		flipFunction = flipFunction, upside.down = TRUE)
	
}

