
netcdf.bbox <- function(file, variable, bbox, outfile = NULL) {
    require(RNetCDF)

    nc <- open.nc(file)
    varinfo <- var.inq.nc(nc, variable)
    vardiminfo <- lapply(varinfo$dimids, function(x) dim.inq.nc(nc, x))
    vardim.df <- do.call("rbind",
                         lapply(vardiminfo, as.data.frame,
                                stringsAsFactors = FALSE))

    vardim.vectors <- sapply(vardim.df$name, function(x) var.get.nc(nc, x))
    nms <- names(vardim.vectors)

    vardim.indexes <- sapply(vardim.vectors, function(x) 1:length(x))

    for (i in 1:nrow(bbox)) {
        vardim.indexes[[nms[i]]] <- which(vardim.vectors[[i]] > bbox[i, 1]
                                          & vardim.vectors[[i]] < bbox[i, 2])
    }

    starts <- unlist(lapply(vardim.indexes, min))
    counts <- unlist(lapply(vardim.indexes, length))
    vardata <- var.get.nc(nc, variable, start = starts, count = counts)

    close.nc(nc)
    for (i in 1:length(vardim.indexes)) {
        vardim.vectors[[i]] <- vardim.vectors[[i]][vardim.indexes[[i]]]
    }

    dimvartype <- "NC_DOUBLE"
    if (!is.null(outfile)) {

        nc <- create.nc(outfile)
        for (i in 1:length(vardim.vectors)) {
            dim.def.nc(nc, names(vardim.vectors)[i], length(vardim.vectors[[i]]))
            var.def.nc(nc, names(vardim.vectors)[i], if (i < 2) dimvartype else "NC_INT", i - 1)
            var.put.nc(nc, names(vardim.vectors)[i], vardim.vectors[[i]])
        }
        var.def.nc(nc, variable, "NC_DOUBLE", 0:(length(dim(vardata))-1))
        var.put.nc(nc, variable, vardata)
        close.nc(nc)
        return(outfile)
    }
    vardim.vectors[[variable]] <- vardata
    vardim.vectors
}
