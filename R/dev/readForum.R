base <- "http://forum.manifold.net/forum/"


setwd("C:/sandbox/Rtemp/georeference")

page <- 1


folder <- paste("p", page * 10, sep = "")

if (!file.exists(folder)) {
    dir.create(folder)

    while(page <=
    pattern <- "<td class='i'>&nbsp;</td>"
    u <- paste(base, "a0p", page, sep = "")
    con <- url(u)
    txt <- readLines(con)
    close(con)
    ind <- grep(pattern, txt)
    ## "<a href='t104424.39'"
    hrefs <- sapply(strsplit(txt[ind], ">"), "[", 5)
    pagerefs <- gsub("'", "", gsub("<a href='", "", hrefs))

    for (pr in pagerefs) {
       u <- paste(base, pr, sep = "")
       con <- url(u)
       pagetext <- readLines(con)
       close(con)
       writeLines(pagetext, paste(file.path(folder, pr), ".html", sep = ""))
   }
}
