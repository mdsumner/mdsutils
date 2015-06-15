ubase <- "http://forum.manifold.net/forum/a"


getTable <- function(x) {
	startpos <- grep('table class', x)
	endpos <- grep('</table>', x)[1]
	if (length(startpos) != 1) stop("problem with table parsing")

	x <- x[(startpos + 1):(endpos - 1)]
	x
}


parseTable <- function(x) {

	titles <- gsub("</a", "", unlist(lapply(strsplit(x, ">"), function(item) item[6])))
	dates <- as.POSIXct(strptime(unlist(lapply(strsplit(x, ">"), function(item) item[9])), "%d-%b-%y"), tz = "GMT")
	posts <- as.numeric(gsub(",", "", gsub("</td", "", unlist(lapply(strsplit(x, ">"), function(item) item[13])))))
	views <- as.numeric(gsub(",", "", gsub("</td", "", unlist(lapply(strsplit(x, ">"), function(item) item[15])))))



       d <- data.frame(titles, dates, posts, views)
       d
}
alast <- FALSE
plast <- FALSE
acount <- 0
pcount <- 1

georef <- NULL

while(!plast) {


	u <- paste(ubase, acount, "p", pcount, sep = "")


	txt <- readLines(u)
	tab <- getTable(txt)
	if (length(tab) < 2) plast <- TRUE
	res <- parseTable(tab[-1])
	georef <- rbind(georef, res)
	pcount <- pcount + 1
	print(u)

}



