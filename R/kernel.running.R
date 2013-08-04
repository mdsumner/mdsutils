#' kernel
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param tr %% ~~Describe \code{tr} here~~
#' @param lats %% ~~Describe \code{lats} here~~
#' @param lons %% ~~Describe \code{lons} here~~
#' @param span %% ~~Describe \code{span} here~~
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
#' function(tr,lats,lons,span=2,scale=1) {
#' 
#'   p <- as.matrix(tr[,c("Lat","Lon")])
#' 
#'   ## grid of lat,lon pts
#'   grid <- as.matrix(expand.grid(lats,lons))
#'   n <- nrow(grid)
#'   ## density estimate
#'   dens <- double(n)
#' 
#' 
#'   for(i in 1:(nrow(p)-span)) {
#' 
#'     p.sub <- p[i:(i+span),]
#'     mu <- apply(p.sub,2,mean)
#'     V <- scale*var(p.sub)
#'     print(V)
#'     dt <- (unclass(tr$Time[i+span])-unclass(tr$Time[i]))/(24*60*60)
#'     dens <- dens + dt*dmvnorm(grid,mu,V)
#'   }
#'   matrix(dens,length(lats),length(lons))
#'   }
#' 
kernel.running <-
function(tr,lats,lons,span=2,scale=1) {

  p <- as.matrix(tr[,c("Lat","Lon")])

  ## grid of lat,lon pts
  grid <- as.matrix(expand.grid(lats,lons))
  n <- nrow(grid)
  ## density estimate
  dens <- double(n)


  for(i in 1:(nrow(p)-span)) {

    p.sub <- p[i:(i+span),]
    mu <- apply(p.sub,2,mean)
    V <- scale*var(p.sub)
    print(V)
    dt <- (unclass(tr$Time[i+span])-unclass(tr$Time[i]))/(24*60*60)
    dens <- dens + dt*dmvnorm(grid,mu,V)
  }
  matrix(dens,length(lats),length(lons))
}

