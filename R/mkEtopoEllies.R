mkEtopoEllies <- 
function () 
{
    cdim <- c(4563, 1635)
    csize <- c(360, 180)/c(10800, 5400)
    cc.offset <- c(95.7666700, -90.0) + csize/2
    gt <- GridTopology(cc.offset, csize, cdim)
    sclEquation <- function(x) {
        x
    }
    Validrange <- c(-11000, 9000)
    bands <- c("topogheight")
    scaledData <- bands[1]
    sdsTemplate <- c("", "")
    timeTemplate <- NULL
    p4 <- CRS("+proj=longlat +datum=WGS84")
    bandNumbers <- NULL
    list(grid = gt, sclEquation = sclEquation, Validrange = Validrange, 
        bands = bands, sdsTemplate = sdsTemplate, timeTemplate = timeTemplate, 
        scaledData = scaledData, p4 = p4, bandNumbers = bandNumbers, 
        upside.down = FALSE)
}