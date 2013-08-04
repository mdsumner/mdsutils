#' ~~function to do ... ~~
#' 
#' ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' ~~ If necessary, more details than the description above ~~
#' 
#' @param directory ~~Describe \code{directory} here~~
#' @param xlim ~~Describe \code{xlim} here~~
#' @param ylim ~~Describe \code{ylim} here~~
#' @param tlim ~~Describe \code{tlim} here~~
#' @param obj ~~Describe \code{obj} here~~
#' @param band ~~Describe \code{band} here~~
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
#' function(directory, xlim, ylim, tlim, obj = NULL, band = obj$bands[1]) {
#' 
#' 	setwd(directory)
#' 	d <- data.frame(files <- list.files())
#' 	d$times <- as.POSIXct(strptime(files, obj$timeTemplate), tz = "GMT")
#' 	d <- d[!is.na(d$times), ]
#' 	d <- d[order(d$times), ]
#' 	if (nrow(d) < 2) {print(d);stop("number of valid files is less than 2")}
#' 	
#' 	tinc <- min(diff(unclass(d$times)))
#' 	if (!(tinc > 0)) stop("files not correctly ordered in time")
#' 	
#' 	if (min(tlim) < min(d$times) | max(tlim) > max(d$times) + tinc) stop("time limit falls outside limit of files")
#' 	
#' 	kp <- which(d$times >= min(tlim) & (d$times + tinc) <= max(tlim))
#' 	d <- d[kp, ]
#' 	## otherwise push on
#' 	
#' 	x <- readData(d$files[1], xlim, ylim, obj)[band]
#' 	if (nrow(d) > 1) for (i in 2:nrow(d)) x[[i]] <- readData(d$files[2], xlim, ylim, obj)[band]
#' 	
#' 	names(x) <- format(d$times, paste(band, "%Y_%m_%d", sep = ""))
#' 	x
#' 	
#' 
#'   }
#' 
`fetchData` <-
function(directory, xlim, ylim, tlim, obj = NULL, band = obj$bands[1]) {

	setwd(directory)
	d <- data.frame(files <- list.files())
	d$times <- as.POSIXct(strptime(files, obj$timeTemplate), tz = "GMT")
	d <- d[!is.na(d$times), ]
	d <- d[order(d$times), ]
	if (nrow(d) < 2) {print(d);stop("number of valid files is less than 2")}
	
	tinc <- min(diff(unclass(d$times)))
	if (!(tinc > 0)) stop("files not correctly ordered in time")
	
	if (min(tlim) < min(d$times) | max(tlim) > max(d$times) + tinc) stop("time limit falls outside limit of files")
	
	kp <- which(d$times >= min(tlim) & (d$times + tinc) <= max(tlim))
	d <- d[kp, ]
	## otherwise push on
	
	x <- readData(d$files[1], xlim, ylim, obj)[band]
	if (nrow(d) > 1) for (i in 2:nrow(d)) x[[i]] <- readData(d$files[2], xlim, ylim, obj)[[band]]
	
	names(x) <- format(d$times, paste(band, "%Y_%m_%d", sep = ""))
	x
	

}

