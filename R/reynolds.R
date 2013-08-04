"reynolds" <-
function (time = ISOdate(1993, 8, 4), xlim = c(0.5, 359.5), ylim = c(-89.5, 
    89.5), dir = "C:\\CONSOLIDATE\\LargeFiles\\DATA\\Reynolds", data = "sst") 
{
    files <- list.files(dir, pattern = "oisst")
    dates <- as.numeric(gsub(".gz", "", gsub("oisst.", "", files)))
    fl <- files[which.min(abs(dates - as.numeric(format(time, 
        "%Y%m%d"))))]

    if (length(grep(".gz", fl)) == 1)
      con <- gzfile(file.path(dir, fl), "rb") else    con <- file(file.path(dir, fl), "rb")
    
    head <- readBin(con, "integer", size = 4, n = 11, endian = "big",  signed = FALSE)
    temps <- readBin(con, "numeric", size = 4, n = 180 * 360,endian = "big")
    junk <- readBin(con, "integer", size = 4, endian = "big", n = 2)

    err <- readBin(con, "numeric", size = 4, n = 180 * 360, endian = "big")
    junk <- readBin(con, "integer", size = 4, endian = "big", n = 2)
    ice <- readBin(con, "integer", size = 1, n = 180 * 360, endian = "big",signed = FALSE)

    close(con)
    lf <- file.path(dir, "lstags.onedeg.dat")
    con <- file(lf, "rb")
    tags <- readBin(con, "numeric", size = 4, n = 180 * 360, 
        endian = "big")
    close(con)
    water <- matrix(tags, nrow = 360)
    sst <- matrix(temps, nrow = 360)
    
    ice <- matrix(ice, nrow = 360)
    err <- matrix(err, nrow = 360)

    lat <- seq(-89.5, 89.5, by = 1)
    lon <- seq(0.5, 359.5, by = 1)


    ## for debugging - should print the same as the example on
    ## ftp://podaac.jpl.nasa.gov/pub/sea_surface_temperature/reynolds/oisst/software/v2/README.v2
    ## when run with file 'oisst.19930804' with no spatial subset
   # test <-  cbind(rep(lon[181], 31),
   #             rev(lat[150:180]),
   #             rev(sst[181, 150:180]),
   #             rev(err[181,150:180]),
   #             rev(ice[181, 150:180]),
   #             rev(water[181, 150:180])*1)
   #colnames(test) <- c("lon", "lat", "sst", "err", "ice", "tagls")
   #print(test)

    sst[water == 0] <- NA
    ice[ice == 122] <- NA
    err[water == 0] <- NA
    subX <- lon >= xlim[1] & lon <= xlim[2]
    subY <- lat >= ylim[1] & lat <= ylim[2]
    if (data == "sst") zz <- sst
    if (data == "ice") zz <- ice
    if (data == "err") zz <- err
    if (data == "water") zz <- water == 1
    if (data == "land") zz <- water == 0
    if (!exists("zz")) stop(data, ": source not known")
    list(x = lon[subX], y = lat[subY], z = zz[subX, subY])
}

