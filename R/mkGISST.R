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
#' 	cdim <- c(360, 180)
#' 	csize <- c(360, 180)/cdim
#' 	cc.offset <- c(0, -90) + csize/2
#' 	gt <- GridTopology(cc.offset, csize, cdim)
#' 	sclEquation <- function(x) {
#' 		Slope <- 0.1
#' 		Intercept <- 0
#' 		x * Slope  + Intercept
#' 	}
#' 	Validrange <- c(0, 1)
#' 	
#' 	bands <- paste("ice", 1:24, sep = "")  ## this is 2 years of data
#' 	scaledData <- bands
#' 	sdsTemplate <- c("", "")
#' 	timeTemplate <- "" ## dunno, something about year since 1919 to 1994
#' 	p4 <- CRS("+proj=longlat +datum=WGS84")
#' 	bandNumbers <- NULL
#' 	list(grid = gt, sclEquation = sclEquation, Validrange = Validrange,
#' 		bands = bands, sdsTemplate = sdsTemplate, timeTemplate = timeTemplate, 
#' 		scaledData = scaledData, p4 = p4, bandNumbers = bandNumbers, upside.down = FALSE)
#' 	
#'   }
#' 
`mkGISST` <-
function() {

	cdim <- c(360, 180)
	csize <- c(360, 180)/cdim
	cc.offset <- c(0, -90) + csize/2
	gt <- GridTopology(cc.offset, csize, cdim)
	sclEquation <- function(x) {
		Slope <- 0.1
		Intercept <- 0
		x * Slope  + Intercept
	}
	Validrange <- c(0, 1)
	
	bands <- paste("ice", 1:24, sep = "")  ## this is 2 years of data
	scaledData <- bands
	sdsTemplate <- c("", "")
	timeTemplate <- "" ## dunno, something about year since 1919 to 1994
	p4 <- CRS("+proj=longlat +datum=WGS84")
	bandNumbers <- NULL
	list(grid = gt, sclEquation = sclEquation, Validrange = Validrange,
		bands = bands, sdsTemplate = sdsTemplate, timeTemplate = timeTemplate, 
		scaledData = scaledData, p4 = p4, bandNumbers = bandNumbers, upside.down = FALSE)
	
}

