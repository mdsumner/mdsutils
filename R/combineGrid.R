#' ~~function to do ... ~~
#' 
#' ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' ~~ If necessary, more details than the description above ~~
#' 
#' @param g1 ~~Describe \code{g1} here~~
#' @param g2 ~~Describe \code{g2} here~~
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
#' function(g1, g2) {
#' 	cdim <- g1@cells.dim
#' 	cdim[1] <- cdim[1] + g2@cells.dim[1]
#' 	GridTopology(g1@cellcentre.offset, g1@cellsize, cdim)
#'   }
#' 
`combineGrid` <-
function(g1, g2) {
	cdim <- g1@cells.dim
	cdim[1] <- cdim[1] + g2@cells.dim[1]
	GridTopology(g1@cellcentre.offset, g1@cellsize, cdim)
}

