infile <- "I:/ccma-geelong-mosaic2_2007dec17_air_vis_35cm_mga54.ecw"
outfile <- "C:/temp/out2.tif"


cmd <- function(infile, outfile, srcwin, outsize = c(1000, 1000)) {
    paste("gdal_translate", infile, outfile, "-srcwin  ", paste(srcwin, collapse = " "),  "-co COMPRESS=LZW -co TILED=YES")
}

library(ff)
library(rgdal)

a <- ff(vmode = "integer", dim = c(12000, 12000, 3), caching = "mmeachflush", dimnames = c(NULL, NULL, NULL))
yoffset <- 200000
plot(NA, xlim = c(0, 12000), ylim = c(0, 12000))
for (i in 1:12) {
    for (j in 1:12) {
        gdalcol <- ((i - 1) * 1000 + 1)
        gdalrow <- yoffset + ((j - 1) * 1000 + 1)
        #system(cmd(infile, outfile, c(gdalcol - 1, gdalrow - 1, 1000, 1000)))
        #d <- readGDAL(outfile)
        rcol <- gdalcol:(gdalcol + 999)
        rrow <- rev(12000 - ((gdalrow:(gdalrow + 999)) - yoffset) + 1)

        for (b in 1:3) {
        #    a[rcol,  rrow, b] <- as.image.SpatialGridDataFrame(d[b])$z
        }

          raster(as.ram(a[rcol, rrow, ])/255, rcol[1], 12000 - max(rrow) + 1, max(rcol), 12000 - min(rrow) + 1)
    }
}



plot(NA, xlim = c(0, 12000), ylim = c(0, 12000))
for (i in 1:12) {
    for (j in 1:12) {
        col <- ((i - 1) * 1000 + 1)
        row <- yoffset + ((j - 1) * 1000 + 1)

        raster(as.ram(a[col:(col + 999), (row:(row + 999)) - yoffset, ])/255, col, row - yoffset, col + 999, row + 999 - yoffset)
    }
}
