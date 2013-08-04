#' gc distance
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param ptA %% ~~Describe \code{ptA} here~~
#' @param ptB %% ~~Describe \code{ptB} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function(ptA,ptB) {
#'   ## Assume pts are in the form (lat,lon)
#'   3437.746771*acos(pmin(1,cos(pi/180*ptA[,1])*
#'                         cos(pi/180*ptB[,1])*
#'                         cos(pi/180*(ptB[,2]-ptA[,2]))+
#'                         sin(pi/180*ptA[,1])*sin(pi/180*ptB[,1])))
#'   }
#' 
gc.dist <-
function(ptA,ptB) {
  ## Assume pts are in the form (lat,lon)
  3437.746771*acos(pmin(1,cos(pi/180*ptA[,1])*
                        cos(pi/180*ptB[,1])*
                        cos(pi/180*(ptB[,2]-ptA[,2]))+
                        sin(pi/180*ptA[,1])*sin(pi/180*ptB[,1])))
}

