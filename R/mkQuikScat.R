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
#' 	cdim <- c(1440, 720)
#' 	csize <- c(360, 180)/cdim
#' 	cc.offset <- c(0, -90) + csize/2
#' 	gt <- GridTopology(cc.offset, csize, cdim)
#' 	sclEquation <- function(x) {
#' 		Slope <- 1#0.01
#' 		Intercept <- 0
#' 		x * Slope  + Intercept
#' 	}
#' 	Validrange <- c(-Inf, Inf) #c(0.0001, 5)  #not fully checked
#' 	flipFunction <- function(x) {
#' 			for (i in names(x)) {
#' 				x[[i]] <- as.vector(as.image.SpatialGridDataFrame(x[i])$z)
#' 			}
#' 			x
#' 			
#' 	}
#' 	bands <- c("asc_avg_wind_speed", "des_avg_wind_speed", 
#' 			"asc_avg_wind_vel_u", "des_avg_wind_vel_u",
#' 			"asc_avg_wind_vel_v", "des_avg_wind_vel_v")
#' 			
#' 	scaledData <- bands
#' 	sdsTemplate <- c("HDF4_SDS:UNKNOWN:\"", "\":")
#' 	timeTemplate <- "" 
#' 	p4 <- CRS("+proj=longlat +datum=WGS84 +over")
#' 	bandNumbers <- 1:length(bands) -1
#' 	list(grid = gt, sclEquation = sclEquation, Validrange = Validrange,
#' 		bands = bands, sdsTemplate = sdsTemplate, timeTemplate = timeTemplate, 
#' 		scaledData = scaledData, p4 = p4, bandNumbers = bandNumbers, upside.down = TRUE, flipFunction = flipFunction)
#' 	
#'   }
#' 
`mkQuikScat` <-
function() {

	cdim <- c(1440, 720)
	csize <- c(360, 180)/cdim
	cc.offset <- c(0, -90) + csize/2
	gt <- GridTopology(cc.offset, csize, cdim)
	sclEquation <- function(x) {
		Slope <- 1#0.01
		Intercept <- 0
		x * Slope  + Intercept
	}
	Validrange <- c(-Inf, Inf) #c(0.0001, 5)  #not fully checked
	flipFunction <- function(x) {
			for (i in names(x)) {
				x[[i]] <- as.vector(as.image.SpatialGridDataFrame(x[i])$z)
			}
			x
			
	}
	bands <- c("asc_avg_wind_speed", "des_avg_wind_speed", 
			"asc_avg_wind_vel_u", "des_avg_wind_vel_u",
			"asc_avg_wind_vel_v", "des_avg_wind_vel_v")
			
	scaledData <- bands
	sdsTemplate <- c("HDF4_SDS:UNKNOWN:\"", "\":")
	timeTemplate <- "" 
	p4 <- CRS("+proj=longlat +datum=WGS84 +over")
	bandNumbers <- 1:length(bands) -1
	list(grid = gt, sclEquation = sclEquation, Validrange = Validrange,
		bands = bands, sdsTemplate = sdsTemplate, timeTemplate = timeTemplate, 
		scaledData = scaledData, p4 = p4, bandNumbers = bandNumbers, upside.down = TRUE, flipFunction = flipFunction)
	
}

