setClass("RectilinearAxes", 
	representation("list"), 
	validity = function(object) {
	#if (length(object@axes.list) != object@dimension) return("axes and dimension should match")
	return(TRUE)
	}
)


setClass("GridTopologyRectilinear", 
	representation("GridTopology", axes = "RectilinearAxes"),
	#prototype = list(new("GridTopology"), axes =   new("RectilinearAxes")),
	validity = function(object) {
		if (length(object@cells.dim) != length(object@axes)) return("rectilinear axis required for each dimension")
		len <- unlist(lapply(object@axes, length), use.names = FALSE)
		if (!isTRUE(all.equal(len, object@cells.dim))) return("length of rectlinear axes must match cells.dim")
		
		return(TRUE)
	}
)


GridTopologyRectilinear = function(grid, rect.axes) {
	new("GridTopologyRectilinear", grid, axes = rect.axes)
}


im.list <- list(x = 1:nrow(volcano), y = 1:ncol(volcano), z = volcano)
im <- image2Grid(im.list)

raxes <- new("RectilinearAxes", im.list[c("x", "y")])
gr <- GridTopologyRectilinear(getGridTopology(im), raxes)

print.GridTopologyRectlinear <- function(object) {
	print("Rectilinear grid, range of steps: ")
	print(sapply(object@axes, function(x) range(diff(x))))
	print(as(object, "GridTopology"))
	
}

setMethod("show", "GridTopologyRectilinear", print.GridTopologyRectlinear)
