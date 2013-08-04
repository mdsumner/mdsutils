#' Lookup MSLA values.
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
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
#' function (x)
#' {
#'     csize <- c(diff(x$x[1:2]), diff(x$y[1:2]))
#'     dimXY <- dim(x$z)
#'     function(xyz, segment = 1:nrow(xy)) {
#'         xs <- xyz[, 1]
#'         ys <- xyz[, 2]
#'         zs <- unclass(xyz[,3])
#'         i <- round((1/diff(x$x[1:2])) * (xs - x$x[1]) + 1)
#'         #j <- round((1/diff(x$y[1:2])) * (ys - x$y[1]) + 1)
#'         j <- findInterval(ys, x$y)
#'         kk <- round((1/diff(unclass(x$gmt[1:2]))) * (zs - unclass(x$gmt[1])) + 1)
#'         f <- vector(mode(x$z), length(xs))
#'         k <- (i > 0 & j > 0 & i <= dimXY[1] & j <= dimXY[2])
#'         n <- nrow(xyz)
#' 
#' 
#'                 f[k] <- x$z[cbind(i[k], j[k], kk[k])]
#' 		f
#'     }
#'   }
#' 
lookupmsla <-
function (x)
{
    csize <- c(diff(x$x[1:2]), diff(x$y[1:2]))
    dimXY <- dim(x$z)
    function(xyz, segment = 1:nrow(xy)) {
        xs <- xyz[, 1]
        ys <- xyz[, 2]
        zs <- unclass(xyz[,3])
        i <- round((1/diff(x$x[1:2])) * (xs - x$x[1]) + 1)
        #j <- round((1/diff(x$y[1:2])) * (ys - x$y[1]) + 1)
        j <- findInterval(ys, x$y)
        kk <- round((1/diff(unclass(x$gmt[1:2]))) * (zs - unclass(x$gmt[1])) + 1)
        f <- vector(mode(x$z), length(xs))
        k <- (i > 0 & j > 0 & i <= dimXY[1] & j <= dimXY[2])
        n <- nrow(xyz)


                f[k] <- x$z[cbind(i[k], j[k], kk[k])]
		f
    }
}

