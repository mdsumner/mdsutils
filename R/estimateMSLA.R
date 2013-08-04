#' Estimate MSLA from at-depth temperatures.
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param xlim %% ~~Describe \code{xlim} here~~
#' @param tm %% ~~Describe \code{tm} here~~
#' @param t500 %% ~~Describe \code{t500} here~~
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
#' function (xlim, tm, t500)
#' {
#'     xm <- mean(xlim)
#'     ilon <- which.min(abs(xm - SOfronts$T_ssh$lon))
#'     ilon <- if (SOfronts$T_ssh$lon[ilon] <= xm)
#'         c(ilon, ilon + 1)
#'     else c(ilon - 1, ilon)
#'     dl <- xm - SOfronts$T_ssh$lon[ilon[1]]
#'     tm <- SOfronts$T_ssh$tm[ilon[1], ] + (SOfronts$T_ssh$tm[ilon[2],
#'         ] - SOfronts$T_ssh$tm[ilon[1], ]) * dl/(SOfronts$T_ssh$lon[ilon[2]] -
#'         SOfronts$T_ssh$lon[ilon[1]])
#'     ts <- SOfronts$T_ssh$ts[ilon[1], ] + (SOfronts$T_ssh$ts[ilon[2],
#'         ] - SOfronts$T_ssh$ts[ilon[1], ]) * dl/(SOfronts$T_ssh$lon[ilon[2]] -
#'         SOfronts$T_ssh$lon[ilon[1]])
#'     ilon <- which.min(abs(xm - SOfronts$ssh_lab$lon))
#'     ilon <- if (SOfronts$ssh_lab$lon[ilon] <= xm)
#'         c(ilon, ilon + 1)
#'     else c(ilon - 1, ilon)
#'     dl <- xm - SOfronts$ssh_lab$lon[ilon[1]]
#'     Conv = SOfronts$ssh_lab$Conv[ilon[1], ] + (SOfronts$ssh_lab$Conv[ilon[2],
#'         ] - SOfronts$ssh_lab$Conv[ilon[1], ]) * dl/(SOfronts$ssh_lab$lon[ilon[2]] -
#'         SOfronts$ssh_lab$lon[ilon[1]])
#'     Xsh = SOfronts$ssh_lab$Xmean1500[ilon[1], ] + (SOfronts$ssh_lab$Xmean1500[ilon[2],
#'         ] - SOfronts$ssh_lab$Xmean1500[ilon[1], ]) * dl/(SOfronts$ssh_lab$lon[ilon[2]] -
#'         SOfronts$ssh_lab$lon[ilon[1]])
#'     ConvI <- approxfun(c(0, Xsh, 3), Conv[c(1, 1:length(Conv),
#'         length(Conv))])(SOfronts$T_ssh$hbin/10)
#'     hbin = SOfronts$T_ssh$hbin * ConvI/10
#'     ok <- !is.na(tm)
#'     ts_loc <- approxfun(tm, ts)(t500)
#'     sshM <- approxfun(tm, hbin)(t500)
#'     sshU <- approxfun(tm, hbin)(t500 + ts_loc)
#'     sshL <- approxfun(tm, hbin)(t500 - ts_loc)
#'     list(mean = sshM, upper = sshU, lower = sshL)
#'   }
#' 
estimateMSLA <-
function (xlim, tm, t500) 
{
    xm <- mean(xlim)
    ilon <- which.min(abs(xm - SOfronts$T_ssh$lon))
    ilon <- if (SOfronts$T_ssh$lon[ilon] <= xm) 
        c(ilon, ilon + 1)
    else c(ilon - 1, ilon)
    dl <- xm - SOfronts$T_ssh$lon[ilon[1]]
    tm <- SOfronts$T_ssh$tm[ilon[1], ] + (SOfronts$T_ssh$tm[ilon[2], 
        ] - SOfronts$T_ssh$tm[ilon[1], ]) * dl/(SOfronts$T_ssh$lon[ilon[2]] - 
        SOfronts$T_ssh$lon[ilon[1]])
    ts <- SOfronts$T_ssh$ts[ilon[1], ] + (SOfronts$T_ssh$ts[ilon[2], 
        ] - SOfronts$T_ssh$ts[ilon[1], ]) * dl/(SOfronts$T_ssh$lon[ilon[2]] - 
        SOfronts$T_ssh$lon[ilon[1]])
    ilon <- which.min(abs(xm - SOfronts$ssh_lab$lon))
    ilon <- if (SOfronts$ssh_lab$lon[ilon] <= xm) 
        c(ilon, ilon + 1)
    else c(ilon - 1, ilon)
    dl <- xm - SOfronts$ssh_lab$lon[ilon[1]]
    Conv = SOfronts$ssh_lab$Conv[ilon[1], ] + (SOfronts$ssh_lab$Conv[ilon[2], 
        ] - SOfronts$ssh_lab$Conv[ilon[1], ]) * dl/(SOfronts$ssh_lab$lon[ilon[2]] - 
        SOfronts$ssh_lab$lon[ilon[1]])
    Xsh = SOfronts$ssh_lab$Xmean1500[ilon[1], ] + (SOfronts$ssh_lab$Xmean1500[ilon[2], 
        ] - SOfronts$ssh_lab$Xmean1500[ilon[1], ]) * dl/(SOfronts$ssh_lab$lon[ilon[2]] - 
        SOfronts$ssh_lab$lon[ilon[1]])
    ConvI <- approxfun(c(0, Xsh, 3), Conv[c(1, 1:length(Conv), 
        length(Conv))])(SOfronts$T_ssh$hbin/10)
    hbin = SOfronts$T_ssh$hbin * ConvI/10
    ok <- !is.na(tm)
    ts_loc <- approxfun(tm, ts)(t500)
    sshM <- approxfun(tm, hbin)(t500)
    sshU <- approxfun(tm, hbin)(t500 + ts_loc)
    sshL <- approxfun(tm, hbin)(t500 - ts_loc)
    list(mean = sshM, upper = sshU, lower = sshL)
}

