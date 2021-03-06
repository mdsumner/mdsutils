#' Create a connection to Manifold map files.
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param map.file %% ~~Describe \code{map.file} here~~
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
#' 
#' 
odbcConnectManifold <- function (map.file)

{

    full.path <- function(filename) {

        fn <- chartr("\\", "/", filename)

        is.abs <- length(grep("^[A-Za-z]:|/", fn)) > 0

        chartr("/", "\\", if (!is.abs)

            file.path(getwd(), filename)

        else filename)

    }

    con <- if (missing(map.file))

        "Driver={Manifold Project Driver (*.map)};Dbq="

    else {

        fp <- full.path(map.file)

        paste("Driver={Manifold Project Driver (*.map)};DBQ=",

            fp, ";DefaultDir=", dirname(fp), ";Unicode=False;Ansi=False;OpenGIS=False;DSN=Default", ";", sep = "")

    }

    odbcDriverConnect(con)

}




manifoldCRS <- function(connection, componentname) {
    sqlQuery(connection, sprintf('SELECT TOP 1 CoordSysToWKT(CoordSys("%s" AS COMPONENT)) AS [CRS] FROM [%s]', componentname, componentname), stringsAsFactors = FALSE)$CRS
}

wktCRS2proj4 <- function(CRS) {
    require(rgdal)
    dsn <- tempdir()
    f <- basename(tempfile())
    writeOGR(SpatialPointsDataFrame(SpatialPoints(cbind(1, 1)), data.frame(x = 1)), dsn, f, "ESRI Shapefile", overwrite = TRUE)
    writeLines(CRS, paste(file.path(dsn, f), ".prj", sep = ""))
    proj4 <- proj4string(readOGR(dsn, f, verbose = FALSE))
    proj4

}


wkt2Spatial <- function(x, id = NULL, p4s = NULL, data = data.frame(x = 1:length(x), row.names = id), ...) {
    ##res <- vector("list", length(x))
    require(rgeos)
    require(maptools)
    if (is.null(id)) id <- as.character(seq_along(x))
    for (i in seq_along(x)) {
        a1 <- readWKT(x[i], id = id[i], p4s = p4s)
        if (i == 1) {
            res <- a1
        } else {
            res <- spRbind(res, a1)
        }
    }
    if (is(res, "SpatialPolygons")) {
        res <- SpatialPolygonsDataFrame(res, data, ...)
    }

    res
}
