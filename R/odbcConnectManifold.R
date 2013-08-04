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

