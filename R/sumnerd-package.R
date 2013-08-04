

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
#'     ## This is the penalized sum of squares
#'     (sum(d^2) + lambda*sum(v^2))/n^2
#'   }
#' 
#'   mn <- nlm(penalized,as.matrix(p.obs[sub,]),...)
#'   m <- n-(first+last)
#'   tr$Lat[sub] <- mn$estimate[1:m]
#'   tr$Lon[sub] <- mn$estimate[m+1:m]
#'   tr
#'   }
#' 
NULL





#' Southern Ocean fronts data
#' 
#' %% ~~ A concise (1-5 lines) description of the dataset. ~~
#' 
#' %% ~~ If necessary, more details than the __description__ above ~~
#' 
#' @name SOfronts
#' @docType data
#' @format The format is: List of 9 $ HOlb : num [1:363, 1:1081] NaN NaN NaN
#' NaN NaN NaN NaN NaN NaN NaN ...  $ Hmean : num [1:363, 1:1081] NaN NaN NaN
#' NaN NaN NaN NaN NaN NaN NaN ...  $ X : num [1:363, 1:1081] 0 0 0 0 0 0 0 0 0
#' 0 ...  $ Xmeann : num [1:12, 1:10] 0.721 0.726 0.729 0.751 0.761 ...  $
#' Xstdn : num [1:12, 1:10] 0.0227 0.0263 0.0168 0.0218 0.0232 ...  $ Y : num
#' [1:363, 1:1081] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...  $ bathG : num
#' [1:363, 1:1081] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...  $ T_ssh :List
#' of 6 ..$ ptm : num [1:9, 1:33] 0.564 1.21 1.251 1.433 NaN ...  ..$ pts : num
#' [1:9, 1:33] 0.2197 0.0273 0.3887 0.0997 NaN ...  ..$ tm : num [1:9, 1:33]
#' 0.546 1.207 1.07 1.43 NaN ...  ..$ ts : num [1:9, 1:33] 0.2349 0.0149 0.5583
#' 0.0457 NaN ...  ..$ lon : num [1:9] -15 45 120 210 262 ...  ..$ hbin: num
#' [1:33] 5 5.5 6 6.5 7 7.5 8 8.5 9 9.5 ...  $ ssh_lab:List of 5 ..$ lon : num
#' [1:14] -15 15 45 75 105 135 165 195 225 255 ...  ..$ Xmean1500: num [1:14,
#' 1:10] 0.504 0.479 0.502 0.504 0.512 ...  ..$ Xmean2500: num [1:14, 1:10]
#' 0.737 0.721 0.726 0.729 0.751 ...  ..$ ifront : num [1:10] 1 2 3 4 5 6 7 8 9
#' 10 ..$ Conv : num [1:14, 1:10] 1.46 1.51 1.45 1.45 1.47 ...
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source %% ~~ reference to a publication or URL from which the data were
#' obtained ~~
#' @keywords datasets
#' @examples
#' 
#' data(SOfronts)
#' ## maybe str(SOfronts) ; plot(SOfronts) ...
#' 
NULL



