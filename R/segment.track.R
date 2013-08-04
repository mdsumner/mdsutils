#' segment
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param tr %% ~~Describe \code{tr} here~~
#' @param breaks %% ~~Describe \code{breaks} here~~
#' @param fun %% ~~Describe \code{fun} here~~
#' @param first %% ~~Describe \code{first} here~~
#' @param last %% ~~Describe \code{last} here~~
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
#' function(tr,breaks,fun,first=T,last=T) {
#' 
#'   ## Define fun
#'   if(missing(fun)) {
#'     fun <- function(seg) {
#'       data.frame(Time=median(unclass(seg$Time), na.rm = TRUE),
#'                  Lat=median(seg$Lat, na.rm = TRUE),
#'                  Lon=median(seg$Lon, na.rm = TRUE))
#'     }
#'   }
#' 
#'   ## Subset of data to summarize
#'   sub <- (1+first):(nrow(tr)-last)
#' 
#'   ## Break into segments and apply summary function
#'   tr.seg <- lapply(split(tr[sub,,drop=FALSE],cut(tr$Time[sub],breaks)),fun)
#'   tr.seg <- do.call("rbind",tr.seg)
#'   class(tr.seg$Time) <- c("POSIXt","POSIXct")
#' browser()
#'   ## Add back first and last
#'   if(first) tr.seg <- rbind(tr[1,],tr.seg)
#'   if(last) tr.seg <- rbind(tr.seg,tr[nrow(tr),])
#'   row.names(tr.seg) <- 1:nrow(tr.seg)
#' 
#'   ## Return segmented track
#'   tr.seg
#'   }
#' 
segment.track <-
function(tr,breaks,fun,first=T,last=T) {

  ## Define fun
  if(missing(fun)) {
    fun <- function(seg) {
      data.frame(Time=median(unclass(seg$Time), na.rm = TRUE),
                 Lat=median(seg$Lat, na.rm = TRUE),
                 Lon=median(seg$Lon, na.rm = TRUE))
    }
  }

  ## Subset of data to summarize
  sub <- (1+first):(nrow(tr)-last)

  ## Break into segments and apply summary function
  tr.seg <- lapply(split(tr[sub,,drop=FALSE],cut(tr$Time[sub],breaks)),fun)
  tr.seg <- do.call("rbind",tr.seg)
  class(tr.seg$Time) <- c("POSIXt","POSIXct")
browser()
  ## Add back first and last
  if(first) tr.seg <- rbind(tr[1,],tr.seg)
  if(last) tr.seg <- rbind(tr.seg,tr[nrow(tr),])
  row.names(tr.seg) <- 1:nrow(tr.seg)

  ## Return segmented track
  tr.seg
}

