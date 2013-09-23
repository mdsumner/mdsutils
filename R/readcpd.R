read.cpd <-
function(x) {
        txt <- readLines(x)
        cold <- read.table(text = sapply(strsplit(grep("^color", txt, value = TRUE), "="), "[", 2), sep = ",")
        cold$value <- as.numeric(sapply(strsplit(grep("^sample", txt, value = TRUE), "="), "[", 2))

        list(breaks = cold$value, col = rgb(cold[,1], cold[,2], cold[, 3], max = 255))
}

##up <- file.path("C:/Users/mdsumner", ".seadas", "beam-ui", "auxdata", "color-palettes")
##fs <- list.files(up, pattern = "cpd$")
##read.cpd(file.path(up, fs[1]))

