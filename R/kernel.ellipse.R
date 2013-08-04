#' kernel
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param tr %% ~~Describe \code{tr} here~~
#' @param lats %% ~~Describe \code{lats} here~~
#' @param lons %% ~~Describe \code{lons} here~~
#' @param vel.max %% ~~Describe \code{vel.max} here~~
#' @param scale %% ~~Describe \code{scale} here~~
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
#' function(tr,lats,lons,vel.max,scale=1) {
#' 
#'   ## grid of lat,lon pts
#'   grid <- as.matrix(expand.grid(lats,lons))
#'   n <- nrow(grid)
#'   ## density estimate
#'   dens <- double(n)
#' 
#'   for(i in 2:nrow(tr)) {
#'     ## The foci of the ellipse
#'     f1 <- c(tr$Lat[i-1],tr$Lon[i-1])
#'     f2 <- c(tr$Lat[i],tr$Lon[i])
#'     dt <- (unclass(tr$Time[i])-unclass(tr$Time[i-1]))/(24*60*60)
#'     print(f1)
#'     print(f2)
#'     print(f2-f1)
#' 
#'     d.max <- vel.max*dt
#'     d <- gc.dist(t(f1),t(f2))
#'     e1 <- (d+d.max)/2
#'     e2 <- sqrt(max(0,d.max^2-d^2))/2
#'     u1 <- (f2-f1)/sqrt(sum((f2-f1)^2))
#'     u2 <- matrix(c(0,1,-1,0),2,2) %*% u1
#'     U <- cbind(u1,u2)
#'     V <- scale*U%*%diag(sqrt(c(e1,e2)),2,2)%*%t(U)
#'     print(V)
#'     mu <-(f1+f2)/2
#'     dens <- dens + dt*dmvnorm(grid,mu,V)
#'   }
#'   matrix(dens,length(lats),length(lons))
#'   }
#' 
kernel.ellipse <-
function(tr,lats,lons,vel.max,scale=1) {

  ## grid of lat,lon pts
  grid <- as.matrix(expand.grid(lats,lons))
  n <- nrow(grid)
  ## density estimate
  dens <- double(n)

  for(i in 2:nrow(tr)) {
    ## The foci of the ellipse
    f1 <- c(tr$Lat[i-1],tr$Lon[i-1])
    f2 <- c(tr$Lat[i],tr$Lon[i])
    dt <- (unclass(tr$Time[i])-unclass(tr$Time[i-1]))/(24*60*60)
    print(f1)
    print(f2)
    print(f2-f1)

    d.max <- vel.max*dt
    d <- gc.dist(t(f1),t(f2))
    e1 <- (d+d.max)/2
    e2 <- sqrt(max(0,d.max^2-d^2))/2
    u1 <- (f2-f1)/sqrt(sum((f2-f1)^2))
    u2 <- matrix(c(0,1,-1,0),2,2) %*% u1
    U <- cbind(u1,u2)
    V <- scale*U%*%diag(sqrt(c(e1,e2)),2,2)%*%t(U)
    print(V)
    mu <-(f1+f2)/2
    dens <- dens + dt*dmvnorm(grid,mu,V)
  }
  matrix(dens,length(lats),length(lons))
}

