library(R.matlab)

readMat.default <-
structure(function (con, maxLength = NULL, fixNames = TRUE, verbose = FALSE,
    sparseMatrixClass = c("Matrix", "SparseM", "matrix"), ...)
{
    this <- list()
    nbrOfBytesRead <- 0
    detectedEndian <- "little"
    ASCII <- c("", "\001", "\002", "\003", "\004", "\005", "\006",
        "\a", "\b", "\t", "\n", "\v", "\f", "\r", "\016", "\017",
        "\020", "\021", "\022", "\023", "\024", "\025", "\026",
        "\027", "\030", "\031", "\032", "\033", "\034", "\035",
        "\036", "\037", " ", "!", "\"", "#", "$", "%", "&", "'",
        "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", "2",
        "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=",
        ">", "?", "@", "A", "B", "C", "D", "E", "F", "G", "H",
        "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
        "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]", "^",
        "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i",
        "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
        "u", "v", "w", "x", "y", "z", "{", "|", "}", "~", "\177",
        "€", "", "‚", "ƒ", "„", "…", "†", "‡", "ˆ", "‰", "Š",
        "‹", "Œ", "", "Ž", "", "", "‘", "’", "“", "”", "•",
        "–", "—", "˜", "™", "š", "›", "œ", "", "ž", "Ÿ", " ",
        "¡", "¢", "£", "¤", "¥", "¦", "§", "¨", "©", "ª", "«",
        "¬", "­", "®", "¯", "°", "±", "²", "³", "´", "µ", "¶",
        "·", "¸", "¹", "º", "»", "¼", "½", "¾", "¿", "À", "Á",
        "Â", "Ã", "Ä", "Å", "Æ", "Ç", "È", "É", "Ê", "Ë", "Ì",
        "Í", "Î", "Ï", "Ð", "Ñ", "Ò", "Ó", "Ô", "Õ", "Ö", "×",
        "Ø", "Ù", "Ú", "Û", "Ü", "Ý", "Þ", "ß", "à", "á", "â",
        "ã", "ä", "å", "æ", "ç", "è", "é", "ê", "ë", "ì", "í",
        "î", "ï", "ð", "ñ", "ò", "ó", "ô", "õ", "ö", "÷", "ø",
        "ù", "ú", "û", "ü", "ý", "þ", "ÿ")
    if (compareVersion(as.character(getRversion()), "2.7.0") <
        0) {
        ASCII[1] <- eval(parse(text = "\"\\000\""))
    }
    intToChar <- function(i) {
        ASCII[i%%256 + 1]
    }
    willRead <- function(nbrOfBytes) {
        if (is.null(maxLength))
            return()
        if (nbrOfBytesRead + nbrOfBytes <= maxLength)
            return()
        stop("Trying to read more bytes than expected from connection. Have read ",
            nbrOfBytesRead, " byte(s) and trying to read another ",
            nbrOfBytes, " byte(s), but expected ", maxLength,
            " byte(s).")
    }
    hasRead <- function(nbrOfBytes) {
        nbrOfBytesRead <<- nbrOfBytesRead + nbrOfBytes
        if (is.null(maxLength))
            return(TRUE)
        return(nbrOfBytesRead <= maxLength)
    }
    isDone <- function() {
        if (is.null(maxLength))
            return(FALSE)
        return(nbrOfBytesRead >= maxLength)
    }
    rawBuffer <- NULL
    fillRawBuffer <- function(need) {
        n <- length(rawBuffer)
        missing <- (need - n)
        if (missing < 0) {
            verbose && cat(verbose, level = -500, "Not filling, have enough data.")
            return(NULL)
        }
        raw <- readBin(con = con, what = raw(), n = missing)
        rawBuffer <<- c(rawBuffer, raw)
        NULL
    }
    eatRawBuffer <- function(eaten) {
        n <- length(rawBuffer)
        if (eaten < n) {
            rawBuffer <<- rawBuffer[(eaten + 1):n]
        }
        else {
            rawBuffer <<- NULL
        }
        NULL
    }
    readBinMat <- function(con, what, size = 1, n, signed = TRUE,
        endian = detectedEndian) {
        if (isDone())
            return(c())
        if (is.na(signed))
            signed <- TRUE
        willRead(size * n)
        fillRawBuffer(size * n)
        bfr <- readBin(con = rawBuffer, what = what, size = size,
            n = n, signed = signed, endian = endian)
        eatRawBuffer(size * n)
        hasRead(length(bfr) * size)
        bfr
    }
    readCharMat <- function(con, nchars) {
        if (isDone())
            return(c())
        willRead(nchars)
        fillRawBuffer(nchars)
        bfr <- rawBuffer[1:nchars]
        bfr <- as.integer(bfr)
        bfr <- intToChar(bfr)
        bfr <- paste(bfr, collapse = "")
        eatRawBuffer(nchars)
        hasRead(nchars)
        bfr
    }
    convertUTF8 <- function(ary) {
        ary <- paste(intToChar(as.integer(ary)), collapse = "")
        Encoding(ary) <- "UTF-8"
        ary
    }
    convertGeneric <- function(ary) {
        ary[ary > 127 | (ary != 0 & ary < 32)] <- NA
        convertUTF8(ary)
    }
    convertUTF16 <- convertUTF32 <- convertGeneric
    if (capabilities("iconv")) {
        utfs <- grep("UTF", iconvlist(), value = TRUE)
        has.utf16 <- utils::head(grep("UTF-?16BE", utfs, value = TRUE),
            n = 1)
        has.utf32 <- utils::head(grep("UTF-?32BE", utfs, value = TRUE),
            n = 1)
        rm(utfs)
        if (length(has.utf16) > 0) {
            convertUTF16 <- function(ary) {
                n <- length(ary)
                ary16 <- paste(intToChar(c(sapply(ary, function(x) {
                  c(x%/%2^8, x%%2^8)
                }))), collapse = "")
                iconv(ary16, has.utf16, "UTF-8")
            }
            convertUTF32 <- function(ary) {
                n <- length(ary)
                ary32 <- paste(intToChar(c(sapply(ary, function(x) {
                  c((x%/%2^24)%%2^8, (x%/%2^16)%%2^8, (x%/%2^8)%%2^8,
                    x%%2^8)
                }))), collapse = "")
                iconv(ary32, has.utf32, "UTF-8")
            }
        }
    }
    charConverter <- function(type) {
        switch(type, miUTF8 = convertUTF8, miUTF16 = convertUTF16,
            miUTF32 = convertUTF32, convertGeneric)
    }
    matToString <- function(ary, type) {
        do.call(charConverter(type), list(ary))
    }
    matToCharArray <- function(ary, type) {
        fn <- charConverter(type)
        sapply(ary, fn)
    }
    pushBackRawMat <- function(con, raw) {
        rawBuffer <<- c(raw, rawBuffer)
        NULL
    }
    asSafeRName <- function(name) {
        if (fixNames) {
            name <- gsub("_", ".", name)
        }
        name
    }
    uncompress <- function(zraw, sizeRatio = 3, delta = 0.9,
        asText = TRUE, ...) {
        if (delta <= 0 || delta >= 1) {
            throw("Argument 'delta' is out of range (0,1): ",
                delta)
        }
        n <- length(zraw)
        unzraw <- NULL
        verbose && printf(verbose, level = -50, "Compress data size: %.3f Mb\n",
            n/1024^2)
        lastException <- NULL
        size <- NULL
        while (is.null(unzraw) && sizeRatio >= 1) {
            size <- sizeRatio * n
            verbose && printf(verbose, level = -50, "Size ratio: %.3f\n",
                sizeRatio)
            lastException <- NULL
            tryCatch({

                gc();gc();gc();gc();
                unzraw <- Rcompression::uncompress(zraw, size = size,
                  asText = asText)
                rm(zraw)
                break
            }, error = function(ex) {
                msg <- ex$message
                if (regexpr("corrupted compressed", msg) != -1) {
                  errorMsg <- paste("Failed to uncompress data: ",
                    msg, sep = "")
                  throw(errorMsg)
                }
                gc()
                lastException <<- ex
            })
            sizeRatio <- delta * sizeRatio
        }
        if (is.null(unzraw)) {
            msg <- lastException$message
            throw(sprintf("Failed to uncompress compressed %d bytes (with smallest initial buffer size of %.3f Mb: %s)",
                n, size/1024^2, msg))
        }
        unzraw
    }
    debugIndent <- 0
    debug <- function(..., sep = "") {
        if (debugIndent > 0)
            cat(paste(rep(" ", length.out = debugIndent), collapse = ""))
        cat(..., sep = sep)
        cat("\n")
    }
    debugPrint <- function(...) {
        print(...)
    }
    debugStr <- function(...) {
        str(...)
    }
    debugEnter <- function(..., indent = +1) {
        debug(..., "...")
        debugIndent <<- debugIndent + indent
    }
    debugExit <- function(..., indent = -1) {
        debugIndent <<- debugIndent + indent
        debug(..., "...done\n")
    }
    isMat4 <- function(MOPT) {
        any(MOPT == 0)
    }
    getMOPT <- function(fourBytes) {
        if (length(fourBytes) != 4)
            stop("Argument 'fourBytes' must a vector of 4 bytes: ",
                length(fourBytes))
        fourBytes <- as.integer(fourBytes)
        neg <- (fourBytes < 0)
        if (any(neg))
            fourBytes[neg] <- fourBytes[neg] + 256
        base <- 256^(0:3)
        MOPT <- c(NA, NA, NA, NA)
        for (endian in c("little", "big")) {
            mopt <- sum(base * fourBytes)
            for (kk in 4:1) {
                MOPT[kk] <- mopt%%10
                mopt <- mopt%/%10
            }
            isMOPT <- (MOPT[1] %in% 0:4 && MOPT[2] == 0 && MOPT[3] %in%
                0:5 && MOPT[4] %in% 0:2)
            if (isMOPT)
                break
            base <- rev(base)
        }
        if (!isMOPT)
            stop("File format error: Not a valid MAT v4. The first four bytes (MOPT) were: ",
                paste(MOPT, collapse = ", "))
        verbose && cat(verbose, level = -50, "Read MOPT bytes: ",
            moptToString(MOPT))
        MOPT
    }
    readMat4 <- function(con, maxLength = NULL, firstFourBytes = NULL) {
        readMat4Header <- function(con, firstFourBytes = NULL) {
            header <- list()
            if (is.null(firstFourBytes)) {
                firstFourBytes <- readBinMat(con, what = integer(),
                  size = 1, n = 4)
            }
            if (length(firstFourBytes) == 0)
                return(NULL)
            MOPT <- getMOPT(firstFourBytes)
            if (MOPT[1] == 0) {
                detectedEndian <<- "little"
            }
            else if (MOPT[1] == 1) {
                detectedEndian <<- "big"
            }
            else if (MOPT[1] %in% 2:4) {
                stop("Looks like a MAT v4 file, but the storage format of numerics (VAX D-float, VAX G-float or Cray) is not supported. Currently only IEEE numeric formats in big or little endian are supported.")
            }
            else {
                stop("Unknown first byte in MOPT header (not in [0,4]): ",
                  paste(MOPT, collapse = ", "))
            }
            header$ocode <- MOPT[2]
            if (MOPT[3] == 0) {
                header$what <- double()
                header$size <- 8
                header$signed <- NA
            }
            else if (MOPT[3] == 1) {
                header$what <- double()
                header$size <- 4
                header$signed <- NA
            }
            else if (MOPT[3] == 2) {
                header$what <- integer()
                header$size <- 4
                header$signed <- TRUE
            }
            else if (MOPT[3] == 3) {
                header$what <- integer()
                header$size <- 2
                header$signed <- TRUE
            }
            else if (MOPT[3] == 4) {
                header$what <- integer()
                header$size <- 2
                header$signed <- FALSE
            }
            else if (MOPT[3] == 5) {
                header$what <- integer()
                header$size <- 1
                header$signed <- FALSE
            }
            else {
                stop("Unknown third byte in MOPT header (not in [0,5]): ",
                  paste(MOPT, collapse = ", "))
            }
            header$matrixType <- "numeric"
            if (MOPT[4] == 0) {
                header$matrixType <- "numeric"
            }
            else if (MOPT[4] == 1) {
                header$matrixType <- "text"
            }
            else if (MOPT[4] == 2) {
                header$matrixType <- "sparse"
            }
            else {
            }
            header$mrows <- readBinMat(con, what = integer(),
                size = 4, n = 1)
            header$ncols <- readBinMat(con, what = integer(),
                size = 4, n = 1)
            verbose && cat(verbose, level = -50, "Matrix dimension: ",
                header$mrows, "x", header$ncols)
            header$imagf <- readBinMat(con, what = integer(),
                size = 4, n = 1)
            verbose && cat(verbose, level = -60, "Matrix contains imaginary values: ",
                as.logical(header$imagf))
            header$namlen <- readBinMat(con, what = integer(),
                size = 4, n = 1)
            verbose && cat(verbose, level = -100, "Matrix name length: ",
                header$namlen - 1)
            header
        }
        readMat4Data <- function(con, header) {
            name <- readCharMat(con, header$namlen)
            verbose && cat(verbose, level = -50, "Matrix name: '",
                name, "'")
            name <- asSafeRName(name)
            verbose && cat(verbose, level = -51, "Matrix safe name: '",
                name, "'")
            n <- header$mrows * header$ncols
            if (header$matrixType == "text") {
                data <- readBinMat(con, what = header$what, size = header$size,
                  signed = header$signed, n = n)
                data <- intToChar(data)
                dim(data) <- c(header$mrows, header$ncols)
                data <- apply(data, MARGIN = 1, FUN = paste,
                  sep = "", collapse = "")
            }
            else if (header$matrixType %in% c("numeric", "sparse")) {
                real <- readBinMat(con, what = header$what, size = header$size,
                  signed = header$signed, n = n)
                if (header$imagf != 0) {
                  verbose && cat(verbose, level = -2, "Reading imaginary part of complex data set.")
                  imag <- readBinMat(con, what = header$what,
                    size = header$size, signed = header$signed,
                    n = n)
                  data <- complex(real = real, imag = imag)
                }
                else {
                  data <- real
                  rm(real)
                }
                dim(data) <- c(header$mrows, header$ncols)
                if (header$matrixType == "sparse") {
                  i <- as.integer(data[, 1])
                  j <- as.integer(data[, 2])
                  s <- data[, 3]
                  rm(data)
                  n <- max(i)
                  m <- max(j)
                  last <- length(i)
                  if (last > 1 && i[last] == i[last - 1] && j[last] ==
                    j[last - 1]) {
                    i <- i[-last]
                    j <- j[-last]
                    s <- s[-last]
                  }
                  if (sparseMatrixClass == "Matrix" && require("Matrix",
                    quietly = TRUE)) {
                    i <- i - as.integer(1)
                    j <- j - as.integer(1)
                    dim <- as.integer(c(n, m))
                    data <- new("dgTMatrix", i = i, j = j, x = s,
                      Dim = dim)
                    data <- as(data, "dgCMatrix")
                  }
                  else if (sparseMatrixClass == "SparseM" &&
                    require("SparseM", quietly = TRUE)) {
                    dim <- as.integer(c(n, m))
                    data <- new("matrix.coo", ra = s, ia = i,
                      ja = j, dimension = dim)
                  }
                  else {
                    pos <- (j - 1) * n + i
                    rm(i, j)
                    data <- matrix(0, nrow = n, ncol = m)
                    data[pos] <- s
                    rm(pos, s)
                  }
                }
            }
            else {
                stop("MAT v4 file format error: Unknown 'type' in header: ",
                  header$matrixType)
            }
            verbose && cat(verbose, level = -60, "Matrix elements:\n")
            verbose && str(verbose, level = -60, data)
            data <- list(data)
            names(data) <- name
            data
        }
        result <- list()
        repeat {
            header <- readMat4Header(con, firstFourBytes = firstFourBytes)
            if (is.null(header))
                break
            data <- readMat4Data(con, header)
            result <- append(result, data)
            rm(data)
            firstFourBytes <- NULL
        }
        header <- list(version = "4", endian = detectedEndian)
        attr(result, "header") <- header
        result
    }
    moptToString <- function(MOPT) {
        if (MOPT[1] == 0)
            mStr <- "IEEE Little Endian (PC, 386, 486, DEC Risc)"
        else if (MOPT[1] == 1)
            mStr <- "IEEE Big Endian (Macintosh, SPARC, Apollo,SGI, HP 9000/300, other Motorola)"
        else if (MOPT[1] == 2)
            mStr <- "VAX D-float"
        else if (MOPT[1] == 3)
            mStr <- "VAX G-float"
        else if (MOPT[1] == 4)
            mStr <- "Cray"
        else mStr <- sprintf("<Unknown value of MOPT[1]. Not in range [0,4]: %d.>",
            as.integer(MOPT[1]))
        if (MOPT[2] == 0)
            oStr <- "Reserved for future use"
        else oStr <- sprintf("<Unknown value of MOPT[2]. Should be 0: %d.>",
            as.integer(MOPT[2]))
        if (MOPT[3] == 0)
            pStr <- "64-bit double"
        else if (MOPT[3] == 1)
            pStr <- "32-bit single"
        else if (MOPT[3] == 2)
            pStr <- "32-bit signed integer"
        else if (MOPT[3] == 3)
            pStr <- "16-bit signed integer"
        else if (MOPT[3] == 4)
            pStr <- "16-bit unsigned integer"
        else if (MOPT[3] == 5)
            pStr <- "8-bit unsigned integer"
        else pStr <- sprintf("<Unknown value of MOPT[3]. Not in range [0,5]: %d.>",
            as.integer(MOPT[3]))
        if (MOPT[4] == 0)
            tStr <- "Numeric (Full) matrix"
        else if (MOPT[4] == 1)
            tStr <- "Text matrix"
        else if (MOPT[4] == 2)
            tStr <- "Sparse matrix"
        else tStr <- sprintf("<Unknown value of MOPT[4]. Not in range [0,2]: %d.>",
            as.integer(MOPT[4]))
        moptStr <- paste("MOPT[1]: ", mStr, ". MOPT[2]: ", oStr,
            ". MOPT[3]: ", pStr, ". MOPT[4]: ", tStr, ".", sep = "")
        moptStr
    }
    readMat5 <- function(con, maxLength = NULL, firstFourBytes = NULL) {
        left <- NA
        readMat5Header <- function(this, firstFourBytes = NULL) {
            if (is.null(firstFourBytes))
                firstFourBytes <- readBinMat(con, what = integer(),
                  size = 1, n = 4)
            MOPT <- firstFourBytes
            if (MOPT[1] %in% 0:4 && MOPT[2] == 0 && MOPT[3] %in%
                0:5 && MOPT[4] %in% 0:2) {
                stop("Detected MAT file format v4. Do not use readMat5() explicitly, but use readMat().")
            }
            description <- c(MOPT, readBinMat(con, what = integer(),
                size = 1, n = 120))
            description <- paste(intToChar(description), collapse = "")
            version <- readBinMat(con, what = integer(), size = 2,
                n = 1, endian = "little")
            endian <- readCharMat(con, nchars = 2)
            if (endian == "MI")
                detectedEndian <<- "big"
            else if (endian == "IM")
                detectedEndian <<- "little"
            else {
                warning("Unknown endian: ", endian, ". Will assume Bigendian.")
                detectedEndian <<- "big"
            }
            if (detectedEndian == "big") {
                hi <- version%/%256
                low <- version%%256
                version <- 256 * low + hi
            }
            if (version == 256) {
                version = "5"
            }
            else {
                warning("Unknown MAT version tag: ", version,
                  ". Will assume version 5.")
                version = as.character(version)
            }
            list(description = description, version = version,
                endian = detectedEndian)
        }
        readMat5DataElement <- function(this) {
            isSigned <- function(type) {
                signed <- c("mxINT8_CLASS", "mxINT16_CLASS",
                  "mxINT32_CLASS")
                signed <- c(signed, "miINT8", "miINT16", "miINT32")
                unsigned <- c("mxUINT8_CLASS", "mxUINT16_CLASS",
                  "mxUINT32_CLASS")
                unsigned <- c(unsigned, "miUINT8", "miUINT16",
                  "miUINT32")
                if (!is.element(type, c(signed, unsigned)))
                  return(NA)
                is.element(type, signed)
            }
            readTag <- function(this) {
                verbose && enter(verbose, level = -80, "Reading Tag")
                on.exit(verbose && exit(verbose))
                type <- readBinMat(con, what = integer(), size = 4,
                  n = 1)
                if (length(type) == 0)
                  return(NULL)
                left <<- left - 4
                knownTypes <- c(miMATRIX = 0, miINT8 = 8, miUINT8 = 8,
                  miINT16 = 16, miUINT16 = 16, miINT32 = 32,
                  miUINT32 = 32, miSINGLE = 32, `--` = NA, miDOUBLE = 64,
                  `--` = NA, `--` = NA, miINT64 = 64, miUINT64 = 64,
                  miMATRIX = NA, miCOMPRESSED = NA, miUTF8 = 8,
                  miUTF16 = 16, miUTF32 = 32)
                knownWhats <- list(miMATRIX = 0, miINT8 = integer(),
                  miUINT8 = integer(), miINT16 = integer(), miUINT16 = integer(),
                  miINT32 = integer(), miUINT32 = integer(),
                  miSINGLE = double(), `--` = NA, miDOUBLE = double(),
                  `--` = NA, `--` = NA, miINT64 = integer(),
                  miUINT64 = integer(), miMATRIX = NA, miUTF8 = integer(),
                  miUTF16 = integer(), miUTF32 = integer())
                nbrOfBytes <- NULL
                tmp <- type
                bytes <- rep(NA, length = 4)
                for (kk in 1:4) {
                  bytes[kk] <- (tmp%%256)
                  tmp <- tmp%/%256
                }
                rm(tmp)
                compressed <- any(bytes[3:4] != 0)
                verbose && cat(verbose, level = -100, "Compressed tag: ",
                  compressed)
                if (compressed) {
                  nbrOfBytes <- type%/%2^16
                  type <- type%%2^16
                  if (detectedEndian == "big") {
                    tmp <- type
                  }
                  if (type + 1 < 1 || type + 1 > length(knownTypes))
                    stop("Unknown data type. Not in range [1,",
                      length(knownTypes), "]: ", type)
                  padding <- 4 - ((nbrOfBytes - 1)%%4 + 1)
                }
                else {
                  nbrOfBytes <- readBinMat(con, what = integer(),
                    size = 4, n = 1)
                  left <<- left - 4
                  padding <- 8 - ((nbrOfBytes - 1)%%8 + 1)
                }
                type <- names(knownTypes)[type + 1]
                sizeOf <- as.integer(knownTypes[type])
                what <- knownWhats[[type]]
                signed <- isSigned(type)
                tag <- list(type = type, signed = signed, sizeOf = sizeOf,
                  what = what, nbrOfBytes = nbrOfBytes, padding = padding,
                  compressed = compressed)
                verbose && print(verbose, level = -100, unlist(tag))
                if (identical(tag$type, "miCOMPRESSED")) {
                  if (!require("Rcompression", quietly = TRUE)) {
                    throw("Cannot read compressed data.  Omegahat.org package 'Rcompression' could not be loaded.  Alternatively, save your data in a non-compressed format by specifying -V6 when calling save() in Matlab or Octave.")
                  }
                  n <- tag$nbrOfBytes
                  zraw <- readBinMat(con = con, what = raw(),
                    n = n)
                  verbose && cat(verbose, level = -110, "Uncompressing ",
                    n, " bytes")
                  unzraw <- uncompress(zraw, asText = FALSE)
                  rm(zraw)
                  verbose && printf(verbose, level = -110, "Inflated %.3f times from %d bytes to %d bytes.\n",
                    length(unzraw)/length(zraw), length(zraw),
                    length(unzraw))
                  pushBackRawMat(con, unzraw)
                  rm(unzraw)
                  gc();gc();gc()
                  ##browser()
                  ## this is just the tag
                  tag <- readTag(this)
##                  browser()
                }
                tag
            }
            readArrayFlags <- function(this) {
                verbose && enter(verbose, level = -70, "Reading Array Flags")
                on.exit(verbose && exit(verbose))
                getBits <- function(i) {
                  ready <- FALSE
                  bits <- c()
                  while (!ready) {
                    bit <- i%%2
                    bits <- c(bits, bit)
                    i <- i%/%2
                    ready <- (i == 0)
                  }
                  bits
                }
                knownTypes <- c(mxCELL_CLASS = NA, mxSTRUCT_CLASS = NA,
                  mxOBJECT_CLASS = NA, mxCHAR_CLASS = 8, mxSPARSE_CLASS = NA,
                  mxDOUBLE_CLASS = NA, mxSINGLE_CLASS = NA, mxINT8_CLASS = 8,
                  mxUINT8_CLASS = 8, mxINT16_CLASS = 16, mxUINT16_CLASS = 16,
                  mxINT32_CLASS = 32, mxUINT32_CLASS = 32)
                arrayFlags <- readBinMat(con, what = integer(),
                  size = 4, n = 1)
                left <<- left - 4
                class <- arrayFlags%%256
                if (class < 1 || class > length(knownTypes)) {
                  stop("Unknown array type (class). Not in [1,",
                    length(knownTypes), "]: ", class)
                }
                class <- names(knownTypes)[class]
                classSize <- knownTypes[class]
                arrayFlags <- arrayFlags%/%256
                flags <- arrayFlags%%256
                flags <- as.logical(getBits(flags + 2^8)[-9])
                logical <- flags[2]
                global <- flags[3]
                complex <- flags[4]
                nzmax <- readBinMat(con, what = integer(), size = 4,
                  n = 1)
                left <<- left - 4
                flags <- list(logical = logical, global = global,
                  complex = complex, class = class, classSize = classSize,
                  nzmax = nzmax)
                verbose && print(verbose, level = -100, unlist(flags[-1]))
                flags
            }
            readDimensionsArray <- function(this) {
                verbose && enter(verbose, level = -70, "Reading Dimensions Array")
                on.exit(verbose && exit(verbose))
                tag <- readTag(this)
                if (tag$type != "miINT32") {
                  throw("Tag type not supported: ", tag$type)
                }
                sizeOf <- tag$sizeOf%/%8
                len <- tag$nbrOfBytes%/%sizeOf
                verbose && cat(verbose, level = -100, "Reading ",
                  len, " integers each of size ", sizeOf, " bytes.")
                dim <- readBinMat(con, what = integer(), size = sizeOf,
                  n = len)
                left <<- left - sizeOf * len
                verbose && cat(verbose, level = -101, "Reading ",
                  tag$padding, " padding bytes.")
                padding <- readBinMat(con, what = integer(),
                  size = 1, n = tag$padding)
                left <<- left - tag$padding
                dimArray <- list(tag = tag, dim = dim)
                verbose && print(verbose, level = -100, list(dim = dim))
                dimArray
            }
            readName <- function(this) {
                verbose && enter(verbose, level = -70, "Reading Array Name")
                on.exit(verbose && exit(verbose))
                tag <- readTag(this)
                sizeOf <- tag$sizeOf%/%8
                nchars <- tag$nbrOfBytes%/%sizeOf
                verbose && cat(verbose, level = -100, "Reading ",
                  nchars, " characters.")
                name <- readBinMat(con, what = tag$what, size = sizeOf,
                  n = nchars)
                name <- matToString(name, tag$type)
                name <- asSafeRName(name)
                left <<- left - nchars
                verbose && cat(verbose, level = -101, "Reading ",
                  tag$padding, " padding bytes.")
                padding <- readBinMat(con, what = integer(),
                  size = 1, n = tag$padding)
                left <<- left - tag$padding
                verbose && cat(verbose, level = -50, "Name: '",
                  name, "'")
                list(tag = tag, name = name)
            }
            readFieldNameLength <- function(this) {
                verbose && enter(verbose, level = -70, "Reading Field Name Length")
                on.exit(verbose && exit(verbose))
                tag <- readTag(this)
                if (tag$type != "miINT32") {
                  throw("Tag type not supported: ", tag$type)
                }
                sizeOf <- tag$sizeOf%/%8
                len <- tag$nbrOfBytes%/%sizeOf
                maxLength <- readBinMat(con, what = integer(),
                  size = sizeOf, n = len)
                left <<- left - len
                padding <- readBinMat(con, what = integer(),
                  size = 1, n = tag$padding)
                left <<- left - tag$padding
                verbose && cat(verbose, level = -100, "Field name length+1: ",
                  maxLength)
                list(tag = tag, maxLength = maxLength)
            }
            readFieldNames <- function(this, maxLength) {
                verbose && enter(verbose, level = -70, "Reading Field Names")
                on.exit(verbose && exit(verbose))
                tag <- readTag(this)
                names <- c()
                sizeOf <- tag$sizeOf%/%8
                nbrOfNames <- tag$nbrOfBytes%/%maxLength
                for (k in seq(length = nbrOfNames)) {
                  name <- readBinMat(con, what = tag$what, size = sizeOf,
                    n = maxLength)
                  name <- matToString(name, tag$type)
                  name <- asSafeRName(name)
                  left <<- left - maxLength
                  names <- c(names, name)
                }
                verbose && cat(verbose, level = -101, "Reading ",
                  tag$padding, " padding bytes.")
                padding <- readBinMat(con, what = integer(),
                  size = 1, n = tag$padding)
                left <<- left - tag$padding
                verbose && cat(verbose, level = -50, "Field names: ",
                  paste(paste("'", names, "'", sep = ""), collapse = ", "))
                list(tag = tag, names = names)
            }
            readFields <- function(this, names) {
                verbose && enter(verbose, level = -70, "Reading Fields")
                on.exit(verbose && exit(verbose))
                fields <- list()
                for (k in seq(names)) {
                  verbose && enter(verbose, level = -3, "Reading field: ",
                    names[k])
                  field <- readMat5DataElement(this)
                  fields <- c(fields, field)
                  verbose && exit(verbose)
                }
                names(fields) <- names
                fields
            }
            readValues <- function(this) {
                verbose && enter(verbose, level = -70, "Reading Values")
                on.exit(verbose && exit(verbose))
                tag <- readTag(this)
                sizeOf <- tag$sizeOf%/%8
                len <- tag$nbrOfBytes%/%sizeOf
                verbose && cat(verbose, level = -100, "Reading ",
                  len, " values each of ", sizeOf, " bytes. In total ",
                  tag$nbrOfBytes, " bytes.")
                value <- readBinMat(con, what = tag$what, size = sizeOf,
                  n = len, signed = tag$signed)
                verbose && str(verbose, level = -102, value)
                left <<- left - sizeOf * len
                verbose && cat(verbose, level = -101, "Reading ",
                  tag$padding, " padding bytes.")
                padding <- readBinMat(con, what = integer(),
                  size = 1, n = tag$padding)
                left <<- left - tag$padding
                list(tag = tag, value = value)
            }
            readMiMATRIX <- function(this, tag) {
                verbose && enter(verbose, level = -70, "Reading miMATRIX")
                on.exit(verbose && exit(verbose))
                verbose && cat(verbose, level = -60, "Argument 'tag':")
                verbose && str(verbose, level = -60, tag)
                tag <- readTag(this)
                if (is.null(tag)) {
                  verbose && cat(verbose, "Nothing more to read. Returning NULL.")
                  verbose && exit(verbose)
                  return(NULL)
                }
                if (tag$type == "miMATRIX") {
                  verbose && enter(verbose, level = -70, "Reading a nested miMATRIX")
                  node <- readMiMATRIX(this, tag)
                  verbose && exit(verbose)
                  verbose && exit(verbose)
                  return(node)
                }
                if (tag$type != "miUINT32") {
                  throw("Tag type not supported: ", tag$type)
                }
                arrayFlags <- readArrayFlags(this)
                arrayFlags$tag <- tag
                arrayFlags$signed <- isSigned(tag$type)
                dimensionsArray <- readDimensionsArray(this)
                arrayName <- readName(this)
                if (arrayFlags$class == "mxCELL_CLASS") {
                  nbrOfCells <- prod(dimensionsArray$dim)
                  verbose && cat(verbose, level = -4, "Reading mxCELL_CLASS with ",
                    nbrOfCells, " cells.")
                  matrix <- list()
                  for (kk in seq(length = nbrOfCells)) {
                    tag <- readTag(this)
                    cell <- readMiMATRIX(this, tag)
                    matrix <- c(matrix, cell)
                  }
                  matrix <- list(matrix)
                  names(matrix) <- arrayName$name
                }
                else if (arrayFlags$class == "mxSTRUCT_CLASS") {
                  nbrOfCells <- prod(dimensionsArray$dim)
                  verbose && cat(verbose, level = -4, "Reading mxSTRUCT_CLASS with ",
                    nbrOfCells, " cells in structure.")
                  maxLength <- readFieldNameLength(this)
                  names <- readFieldNames(this, maxLength = maxLength$maxLength)
                  verbose && cat(verbose, level = -100, "Field names: ",
                    paste(names$names, collapse = ", "))
                  nbrOfFields <- length(names$names)
                  matrix <- list()
                  for (kk in seq(length = nbrOfCells)) {
                    fields <- readFields(this, names = names$names)
                    matrix <- c(matrix, fields)
                  }
                  names(matrix) <- NULL
                  dim <- c(nbrOfFields, dimensionsArray$dim)
                  if (prod(dim) > 0) {
                    matrix <- structure(matrix, dim = dim)
                    dimnames <- rep(list(NULL), length(dim(matrix)))
                    dimnames[[1]] <- names$names
                    dimnames(matrix) <- dimnames
                  }
                  matrix <- list(matrix)
                  names(matrix) <- arrayName$name
                  verbose && cat(verbose, level = -60, "Read a 'struct':")
                  verbose && str(verbose, level = -60, matrix)
                }
                else if (arrayFlags$class == "mxOBJECT_CLASS") {
                  className <- readName(this)$name
                  maxLength <- readFieldNameLength(this)
                  verbose && cat(verbose, level = -4, "Reading mxOBJECT_CLASS of class '",
                    className, "' with ", maxLength, " fields.")
                  names <- readFieldNames(this, maxLength = maxLength$maxLength)
                  fields <- readFields(this, names = names$names)
                  class(fields) <- className
                  matrix <- list(fields)
                  names(matrix) <- arrayName$name
                }
                else if (arrayFlags$complex) {
                  verbose && enter(verbose, level = -4, "Reading complex matrix.")
                  pr <- readValues(this)
                  if (left > 0)
                    pi <- readValues(this)
                  matrix <- complex(real = pr$value, imaginary = pi$value)
                  dim(matrix) <- dimensionsArray$dim
                  verbose && str(verbose, level = -10, matrix)
                  matrix <- list(matrix)
                  names(matrix) <- arrayName$name
                  verbose && exit(verbose, suffix = paste("...done: '",
                    names(matrix), "' [", mode(matrix), ": ",
                    paste(dim(matrix), collapse = "x"), " elements]",
                    sep = ""))
                }
                else if (arrayFlags$class == "mxSPARSE_CLASS") {
                  nrow <- dimensionsArray$dim[1]
                  ncol <- dimensionsArray$dim[2]
                  verbose && cat(verbose, level = -4, "Reading mxSPARSE_CLASS ",
                    nrow, "x", ncol, " matrix.")
                  nzmax <- arrayFlags$nzmax
                  ir <- c()
                  jc <- c()
                  pr <- c()
                  if (nzmax > 0) {
                    ir <- readValues(this)$value
                    ir <- ir + 1
                    if (any(ir < 1 | ir > nrow)) {
                      stop("MAT v5 file format error: Some elements in row vector 'ir' (sparse arrays) are out of range [1,",
                        nrow, "].")
                    }
                    jc <- readValues(this)$value
                    if (length(jc) != ncol + 1) {
                      stop("MAT v5 file format error: Length of column vector 'jc' (sparse arrays) is not ",
                        ncol, "+1 as expected: ", length(jc))
                    }
                    pr <- readValues(this)$value
                    verbose && str(verbose, level = -102, ir)
                    verbose && str(verbose, level = -102, jc)
                    verbose && str(verbose, level = -102, pr)
                    if (arrayFlags$complex) {
                      pi <- readValues(this)$value
                      verbose && str(verbose, level = -102, pi)
                    }
                    nzmax <- min(nzmax, jc[ncol + 1])
                    if (nzmax < length(ir)) {
                      ir <- ir[1:nzmax]
                    }
                    if (nzmax < length(pr)) {
                      pr <- pr[1:nzmax]
                    }
                    if (arrayFlags$complex) {
                      if (nzmax < length(pi)) {
                        pi <- pi[1:nzmax]
                      }
                      pr <- complex(real = pr, imaginary = pi)
                      rm(pi)
                    }
                  }
                  if (sparseMatrixClass == "Matrix" && require("Matrix",
                    quietly = TRUE)) {
                    if (is.integer(pr) || is.logical(pr)) {
                      pr <- as.double(pr)
                    }
                    matrix <- new("dgCMatrix", x = pr, p = as.integer(jc),
                      i = as.integer(ir - 1), Dim = as.integer(c(nrow,
                        ncol)))
                    matrix <- list(matrix)
                    names(matrix) <- arrayName$name
                  }
                  else if (sparseMatrixClass == "SparseM" &&
                    require("SparseM", quietly = TRUE)) {
                    if (is.integer(pr) || is.logical(pr)) {
                      pr <- as.double(pr)
                    }
                    matrix <- new("matrix.csc", ra = pr, ja = as.integer(ir),
                      ia = as.integer(jc + 1), dimension = as.integer(c(nrow,
                        ncol)))
                    matrix <- list(matrix)
                    names(matrix) <- arrayName$name
                  }
                  else {
                    matrix <- matrix(0, nrow = nrow, ncol = ncol)
                    attr(matrix, "name") <- arrayName$name
                    for (col in seq(length = length(jc) - 1)) {
                      first <- jc[col]
                      last <- jc[col + 1] - 1
                      idx <- seq(from = first, to = last)
                      value <- pr[idx]
                      row <- ir[idx]
                      ok <- is.finite(row)
                      row <- row[ok]
                      value <- value[ok]
                      matrix[row, col] <- value
                    }
                    rm(ir, jc, first, last, idx, value, row)
                    matrix <- list(matrix)
                    names(matrix) <- arrayName$name
                  }
                }
                else {
                  data <- readValues(this)
                  matrix <- data$value
                  verbose && cat(verbose, level = -5, "Converting to ",
                    arrayFlags$class, " matrix.")
                  if (arrayFlags$class == "mxDOUBLE_CLASS") {
                    matrix <- as.double(matrix)
                    dim(matrix) <- dimensionsArray$dim
                  }
                  else if (arrayFlags$class == "mxSINGLE_CLASS") {
                    matrix <- as.single(matrix)
                    dim(matrix) <- dimensionsArray$dim
                  }
                  else if (is.element(arrayFlags$class, c("mxINT8_CLASS",
                    "mxUINT8_CLASS", "mxINT16_CLASS", "mxUINT16_CLASS",
                    "mxINT32_CLASS", "mxUINT32_CLASS"))) {
                    matrix <- as.integer(matrix)
                    dim(matrix) <- dimensionsArray$dim
                  }
                  else if (arrayFlags$class == "mxCHAR_CLASS") {
                    matrix <- matToCharArray(matrix, tag$type)
                    dim(matrix) <- dimensionsArray$dim
                    matrix <- apply(matrix, MARGIN = 1, FUN = paste,
                      collapse = "")
                    matrix <- as.matrix(matrix)
                  }
                  else {
                    stop("Unknown or unsupported class id in array flags: ",
                      arrayFlags$class)
                  }
                  matrix <- list(matrix)
                  names(matrix) <- arrayName$name
                }
                matrix
            }
            tag <- readTag(this)
            if (is.null(tag))
                return(NULL)
            if (tag$nbrOfBytes == 0)
                return(list(NULL))
            left <<- tag$nbrOfBytes
            if (tag$type == "miMATRIX") {
                verbose && enter(verbose, level = -3, "Reading (outer) miMATRIX")
                data <- readMiMATRIX(this, tag)
                verbose && str(verbose, level = -4, data)
                verbose && exit(verbose)
            }
            else {
                verbose && printf(verbose, level = -3, "Reading (outer) %.0f integers",
                  tag$nbrOfBytes)
                data <- readBinMat(con, what = integer(), size = 1,
                  n = tag$nbrOfBytes, signed = tag$signed)
            }
            data
        }
        detectedEndian <<- "little"
        header <- readMat5Header(this, firstFourBytes = firstFourBytes)
        verbose && cat(verbose, level = -100, "Read MAT v5 header:")
        verbose && print(verbose, level = -100, header)
        verbose && cat(verbose, level = -100, "Endian: ", detectedEndian)
        result <- list()
        repeat {
            verbose && enter(verbose, level = -2, "Reading data element")
            data <- readMat5DataElement(this)
            if (is.null(data)) {
                verbose && exit(verbose)
                break
            }
            result <- append(result, data)
            verbose && exit(verbose, suffix = paste("...done: '",
                names(data), "' [", mode(data[[1]]), ": ", paste(dim(data[[1]]),
                  collapse = "x"), "]", sep = ""))
        }
        attr(result, "header") <- header
        result
    }
    sparseMatrixClass <- match.arg(sparseMatrixClass)
    if (inherits(verbose, "Verbose")) {
    }
    else if (is.numeric(verbose)) {
        require("R.utils") || throw("Package not available: R.utils")
        verbose <- Verbose(threshold = verbose)
    }
    else {
        verbose <- as.logical(verbose)
        if (verbose) {
            require("R.utils") || throw("Package not available: R.utils")
            verbose <- Verbose(threshold = -1)
        }
    }
    if (inherits(con, "connection")) {
        if (!isOpen(con)) {
            verbose && cat(verbose, level = -1, "Opens binary connection.")
            open(con, open = "rb")
            on.exit({
                close(con)
                verbose && cat(verbose, level = -1, "Binary connection closed.")
            })
        }
    }
    else {
        con <- as.character(con)
        verbose && cat(verbose, level = -1, "Opens binary file: ",
            con)
        con <- file(con, open = "rb")
        on.exit({
            close(con)
            verbose && cat(verbose, level = -1, "Binary file closed.")
        })
    }
    if (summary(con)$text != "binary")
        stop("Can only read a MAT file structure from a *binary* connection.")
    nbrOfBytesRead <- 0
    firstFourBytes <- readBinMat(con, what = integer(), size = 1,
        n = 4)
    if (is.null(firstFourBytes))
        stop("MAT file format error: Nothing to read. Empty input stream.")
    if (isMat4(firstFourBytes)) {
        verbose && cat(verbose, level = 0, "Trying to read MAT v4 file stream...")
        readMat4(con, firstFourBytes = firstFourBytes, maxLength = maxLength)
    }
    else {
        verbose && cat(verbose, level = 0, "Trying to read MAT v5 file stream...")
        readMat5(con, firstFourBytes = firstFourBytes, maxLength = maxLength)
    }
}, modifiers = "public")










setwd("G:/GEM/raw_matlab/temp")
d <- readMat("temp_CARS_MDT_divby2_depth_withseason25.mat")

