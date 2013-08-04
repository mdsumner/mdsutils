mkEtopoElliesWorld2 <- 
function () 
{
   ## MIROUNGA:C:/spatdata/topodata/Etopo-ElliesWorld2.tif"
  ## grid specification
    cdim <- c(4563, 1635)
    csize <- c( 0.03333333,  0.03333333)
    cc.offset <- c(95.76667, -90 + csize[2]/2)
   
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