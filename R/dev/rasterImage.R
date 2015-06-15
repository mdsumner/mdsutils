x <- df[df$problem == levels(df$problem)[2], ]

x <- list(x = sort(unique(x[,1])), y = sort(unique(x[,2])), z = matrix(as.numeric(x[,3]), 201))

x$z <- x$z - min(x$z)
x$z <- x$z/max(x$z)

pdf()
image(x$z)
dev.off()
file.info("Rplots.pdf")$size/1e6
1.17

pdf()
plot(0, type = "n", xlim = range(x$x), ylim = range(x$y))
rasterImage(x$z, min(x$x), min(x$y), max(x$x), max(x$y), interpolate = FALSE)
dev.off()
file.info("Rplots.pdf")$size/1e6
## 2.11
0.145173



