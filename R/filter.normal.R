#' filter
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param tr %% ~~Describe \code{tr} here~~
#' @param lambda %% ~~Describe \code{lambda} here~~
#' @param first %% ~~Describe \code{first} here~~
#' @param last %% ~~Describe \code{last} here~~
#' @param \dots %% ~~Describe \code{\dots} here~~
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
#' function(tr,lambda,first=T,last=T,...) {
#' 
#'   ## Number of points and subset
#'   n <- nrow(tr)
#'   sub <- (1+first):(n-last)
#' 
#'   ## Observed points
#'   p.obs <- as.matrix(tr[,c("Lat","Lon")])
#' 
#'   ## Time intervals (in days) between obs
#'   dt <- diff(unclass(tr$Time)/(24*60*60))
#' 
#'   penalized <- function(x) {
#' 
#'     ## Form smoothed track
#'     p <- p.obs
#'     p[sub,] <- x
#' 
#'     ## Velocities between smoothed points
#'     v <- gc.dist(p[-n,],p[-1,])/dt
#' 
#'     ## Distances from smoothed points to observations
#'     d <- gc.dist(p,p.obs)
#' 
#'     ## This is the penalized
#'     sum(dnorm(d,log=T),dnorm(v,sigma=lambda,log=T))/n
#'   }
#' 
#'   mn <- nlm(penalized,as.matrix(p.obs[sub,]),...)
#'   m <- n-(first+last)
#'   tr$Lat[sub] <- mn$estimate[1:m]
#'   tr$Lon[sub] <- mn$estimate[m+1:m]
#'   tr
#'   }
#' 
filter.normal <-
function(tr,lambda,first=T,last=T,...) {

  ## Number of points and subset
  n <- nrow(tr)
  sub <- (1+first):(n-last)

  ## Observed points
  p.obs <- as.matrix(tr[,c("Lat","Lon")])

  ## Time intervals (in days) between obs
  dt <- diff(unclass(tr$Time)/(24*60*60))

  penalized <- function(x) {

    ## Form smoothed track
    p <- p.obs
    p[sub,] <- x

    ## Velocities between smoothed points
    v <- gc.dist(p[-n,],p[-1,])/dt

    ## Distances from smoothed points to observations
    d <- gc.dist(p,p.obs)

    ## This is the penalized
    sum(dnorm(d,log=T),dnorm(v,sigma=lambda,log=T))/n
  }

  mn <- nlm(penalized,as.matrix(p.obs[sub,]),...)
  m <- n-(first+last)
  tr$Lat[sub] <- mn$estimate[1:m]
  tr$Lon[sub] <- mn$estimate[m+1:m]
  tr
}

