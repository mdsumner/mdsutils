mllgridlines <- 
  function (obj, easts, norths, ndiscr = 20, lty = 2, offset = 0.5, 
            side = "WS", llcrs = "+proj=longlat +datum=WGS84", plotLines = TRUE, 
            plotLabels = TRUE, bbtest = TRUE, ...) 
  {
    obj_ll <- spTransform(obj, CRS(llcrs))
    if (missing(easts)) 
      easts = pretty(bbox(obj_ll)[1, ])
    if (missing(norths)) 
      norths = pretty(bbox(obj_ll)[2, ])
    grd <- mgridlines(obj_ll, easts = easts, norths = norths, 
                      ndiscr = ndiscr, bbtest = bbtest)
    grd_x <- spTransform(grd, CRS(proj4string(obj)))
    if (plotLines) 
      plot(grd_x, add = TRUE, lty = lty, ...)
    if (packageVersion("sp") >= "0.9.84") {
      grdat_ll <- mgridat(obj_ll, easts = easts, norths = norths, 
                          side = side, offset = offset)
    }
    else {
      grdat_ll <- mgridat(obj_ll, easts = easts, norths = norths, 
                          offset = offset)
    }
    grdat_x <- spTransform(grdat_ll, CRS(proj4string(obj)))
    if (plotLabels) 
      text(coordinates(grdat_x), labels = parse(text = grdat_x$labels), 
           pos = grdat_x$pos, offset = grdat_x$offset, ...)
  }


mgridlines <- function (x, easts = pretty(bbox(x)[1, ]), norths = pretty(bbox(x)[2, 
                                                                                 ]), ndiscr = 20, bbtest = TRUE) 
{
  bb = bbox(x)
  ##    easts <- easts[easts > bb[1, 1] & easts < bb[1, 2]]
  if (bbtest)    {
    easts <- easts[easts >= bb[1, 1] & easts <= bb[1, 2]]
  }
  
  eastlist <- vector(mode = "list", length = length(easts))
  ## for (i in 1:length(easts)) eastlist[[i]] <- Line(cbind(rep(easts[i], 
  ##     ndiscr), seq(bb[2, 1], bb[2, 2], length.out = ndiscr)))
  for (i in 1:length(easts)) eastlist[[i]] <- Line(cbind(rep(easts[i], 
                                                             ndiscr), seq(min(norths), max(norths), length.out = ndiscr)))
  
  ##    norths <- norths[norths > bb[2, 1] & norths < bb[2, 2]]
  if (bbtest) {
    norths <- norths[norths >= bb[2, 1] & norths <= bb[2, 2]]
  }
  northlist <- vector(mode = "list", length = length(norths))
  # for (i in 1:length(norths)) northlist[[i]] <- Line(cbind(seq(bb[1, 
  #     1], bb[1, 2], length.out = ndiscr), rep(norths[i], ndiscr)))
  for (i in 1:length(norths)) northlist[[i]] <- Line(cbind(seq(min(easts), max(easts), 
                                                               length.out = ndiscr), rep(norths[i], ndiscr)))
  SpatialLines(list(Lines(northlist, "NS"), Lines(eastlist, 
                                                  "EW")), CRS(proj4string(x)))
}


mgridat <- 
  function (x, easts = pretty(bbox(x)[1, ]), norths = pretty(bbox(x)[2, 
                                                                     ]), offset = 0.5, side = "WS") 
  {
    isp = is.projected(x)
    if (is.na(isp) || isp) 
      stop("x must not be projected")
    bb = bbox(x)
    ac <- ifelse(side == "WS", 1L, 2L)
    easts <- easts[easts > bb[1, 1] & easts < bb[1, 2]]
    norths <- norths[norths > bb[2, 1] & norths < bb[2, 2]]
    a1 <- cbind(easts, rep(bb[2, ac], length(easts)))
    a1lab <- degreeLabelsEW(a1[, 1])
    a2 <- cbind(rep(bb[1, ac], length(norths)), norths)
    a2lab <- degreeLabelsNS(a2[, 2])
    as <- SpatialPoints(rbind(a1, a2), CRS(proj4string(x)))
    res <- SpatialPointsDataFrame(as, data.frame(labels = c(a1lab, 
                                                            a2lab), pos = c(rep(1L + ((ac - 1) * 2), length(easts)), 
                                                                            rep(2L + ((ac - 1) * 2), length(norths))), offset = rep(offset, 
                                                                                                                                    length(easts) + length(norths)), stringsAsFactors = FALSE))
    res
  }
