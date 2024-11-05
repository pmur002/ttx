
initTTX <- function() {
    ttx <- Sys.which("ttx")
    if (nchar(ttx) > 0) {
        version <- system("ttx --version", intern=TRUE)
        set("ttxVersion", version)
    }
}

ttxVersion <- function() {
    get("ttxVersion")
}

ttxAvailable <- function() {
    !is.null(ttxVersion())
}

ttx <- function(options, intern=FALSE) {
    if (getOption("ttx.quiet")) {
        options <- paste0(" -q ", options)
    }
    if (ttxAvailable()) {
        system(paste0("ttx ", options), intern=intern)
    } else {
        stop("ttx not installed")
    }
}

################################################################################
## TTX cache (to avoid loading TTX files over and over again)

getTTXcache <- function() {
    cache <- get("ttxCache")
    if (is.null(cache))
        set("ttxCache", list())
    cache
}

cacheTTX <- function(ttxFile, ttx) {
    cache <- getTTXcache()
    cache[[ttxFile]] <- ttx
    set("ttxCache", cache)
}

getTTX <- function(ttxFile) {
    cache <- getTTXcache()
    if (is.null(cache) ||
        is.null(cache[[ttxFile]])) {
        ttx <- read_xml(ttxFile)
        cacheTTX(ttxFile, ttx)        
    } else {
        ttx <- cache[[ttxFile]]
    }
    ttx
}

################################################################################
## Generate RDS files (from TTX files)

msg <- function(...) {
    if (!getOption("ttx.quiet")) {
        message(...)
    }
}

## Vector of glyph indices with names = glyph names
## (track nGlyph to allow for gaps in glyph indices)
generateGlyphOrder <- function(ttx, rdsFile) {
    msg(paste0("Generating ", rdsFile, " ..."))
    glyphs <- xml_find_all(ttx, "//GlyphID")
    name <- xml_attr(glyphs, "name")
    id <- as.numeric(xml_attr(glyphs, "id"))
    names(id) <- name
    ## '+ 1' because glyph ids are zero-based in XML
    ## BUT I want to use them to index a (one-based) R vector or matrix
    attr(id, "nGlyph") <- max(id) + 1
    saveRDS(id, rdsFile)
}

## Matrix of width and left side bearing *in glyph index order*
generateHMTX <- function(ttx, fontfile, suffix, rdsFile) {
    msg(paste0("Generating ", rdsFile, " ..."))
    glyphOrder <- getGlyphOrderTable(fontfile, suffix)
    nGlyph <- attr(glyphOrder, "nGlyph")
    metrics <- xml_find_all(ttx, "//mtx")
    name <- xml_attr(metrics, "name")
    width <- numeric(nGlyph)
    ## '+ 1' because glyph ids are zero-based in XML
    ## BUT I want to use them to index a (one-based) R vector or matrix
    width[glyphOrder[name] + 1] <- as.numeric(xml_attr(metrics, "width"))
    lsb <- numeric(nGlyph)
    lsb[glyphOrder[name] + 1] <- as.numeric(xml_attr(metrics, "lsb"))
    hmtx <- cbind(width, lsb)
    saveRDS(hmtx, rdsFile)
}

## Matrix of height and top side bearing *in glyph index order*
generateVMTX <- function(ttx, fontfile, suffix, rdsFile) {
    msg(paste0("Generating ", rdsFile, " ..."))
    glyphOrder <- getGlyphOrderTable(fontfile, suffix)
    nGlyph <- attr(glyphOrder, "nGlyph")
    metrics <- xml_find_all(ttx, "//mtx")
    name <- xml_attr(metrics, "name")
    height <- numeric(nGlyph)
    height[glyphOrder[name] + 1] <- as.numeric(xml_attr(metrics, "height"))
    tsb <- numeric(nGlyph)
    tsb[glyphOrder[name] + 1] <- as.numeric(xml_attr(metrics, "tsb"))
    vmtx <- cbind(height, tsb)
    saveRDS(vmtx, rdsFile)
}

## Matrix of xmin, xmax, ymin, ymax *in glyph index order*
generateGLYF <- function(ttx, fontfile, suffix, rdsFile) {
    msg(paste0("Generating ", rdsFile, " ..."))
    metrics <- xml_find_all(ttx, "//TTGlyph")
    name <- xml_attr(metrics, "name")
    if (grepl("-FROM-OTF", fontfile)) {
        ## Need to map metrics from .ttf to glyph order of original .otf
        otfFile <- gsub("-FROM-OTF[.]ttf", ".otf", fontfile)
        glyphOrder <- getGlyphOrderTable(otfFile, "otf")
        ## converted .ttf may have glyphs not in original .otf (e.g., ".null")
        subset <- name %in% names(glyphOrder)
        name <- name[subset]
        metrics <- metrics[subset]
        glyphOrder <- glyphOrder[name]
        attr(glyphOrder, "nGlyph") <- length(glyphOrder)
    } else {
        glyphOrder <- getGlyphOrderTable(fontfile, suffix)
    }
    nGlyph <- attr(glyphOrder, "nGlyph")
    xmin <- numeric(nGlyph)
    xmin[glyphOrder[name] + 1] <- as.numeric(xml_attr(metrics, "xMin"))
    xmax <- numeric(nGlyph)
    xmax[glyphOrder[name] + 1] <- as.numeric(xml_attr(metrics, "xMax"))
    ymin <- numeric(nGlyph)
    ymin[glyphOrder[name] + 1] <- as.numeric(xml_attr(metrics, "yMin"))
    ymax <- numeric(nGlyph)
    ymax[glyphOrder[name] + 1] <- as.numeric(xml_attr(metrics, "yMax"))
    glyf <- cbind(xmin, xmax, ymin, ymax)
    saveRDS(glyf, rdsFile)
}

generateRDS <- function(table, fontfile, suffix, ttx, rdsFile) {
    switch(table,
           GlyphOrder=generateGlyphOrder(ttx, rdsFile),
           hmtx=generateHMTX(ttx, fontfile, suffix, rdsFile),
           vmtx=generateVMTX(ttx, fontfile, suffix, rdsFile),
           glyf=generateGLYF(ttx, fontfile, suffix, rdsFile))
}

## Cache RDS instead of TTX
getRDS <- function(rdsFile) {
    cache <- getTTXcache()
    if (is.null(cache) ||
        is.null(cache[[rdsFile]])) {
        rds <- readRDS(rdsFile)
        cacheTTX(rdsFile, rds)
    } else {
        rds <- cache[[rdsFile]]
    }
    rds 
}

################################################################################
## Take copy of font file and place it in cache directory for taking apart
## (and get font name and suffix)

## User can set option for font directory that is "permanent"
## or a temporary one will be created per R session.
ttxFontDir <- function() {
    fontDir <- getOption("ttx.cacheDir")
    if (!is.null(fontDir)) {
        if (dir.exists(fontDir)) {
            return(fontDir)
        } else {
            warning(paste0("Font cache directory ", fontDir,
                           " does not exist; ",
                           "using temporary directory instead."))
        }
    }
    fontDir <- file.path(tempdir(), "TTXfonts")
    if (!dir.exists(fontDir)) {
        dir.create(fontDir)
    }
    fontDir
}

ttxFontFile <- function(fontpath) {
    fontDir <- ttxFontDir()
    filename <- basename(fontpath)
    fontsuffix <- "[.](ttf|otf)$"
    if (!grepl(fontsuffix, filename))
        warning("Unrecognised font suffix")
    filesuffix <- gsub(paste0(".+", fontsuffix), "\\1", filename)
    filestub <- gsub(fontsuffix, "", filename)
    fontfile <- file.path(fontDir, filename)
    if (!file.exists(fontfile)) {
        file.copy(fontpath, fontDir)
    }
    list(file=fontfile, suffix=filesuffix)
}

################################################################################
## List all tables
tableList <- function(fontfile) {
    listing <- ttx(paste0("-l ", shQuote(fontfile)), intern=TRUE) 
    gsub(" .+", "",
         gsub("^ +", "",
              listing[-c(1:3, length(listing))]))
}

## Get a single table
getTable <- function(table, fontfile, suffix, replace=table) {
    ttxfile <- gsub(paste0("[.]", suffix, "$"),
                    paste0("-", replace, ".ttx"), fontfile)
    if (!file.exists(ttxfile)) {
        ## For "glyf" table, check first whether it is available in the font
        ## and, if not (and font is .otf), convert to .ttf and generate table
        ## from that.
        if (table == "glyf") {
            tables <- tableList(fontfile)
            if (!("glyf" %in% tables)) {
                if (fontForgeAvailable() && suffix == "otf") {
                    ttfFile <- gsub("[.]otf$", "-FROM-OTF.ttf", fontfile)
                    otf2ttf(fontfile, ttfFile)
                    fontfile <- ttfFile
                    suffix <- "ttf"
                } else {
                    warning("glyf table unavailable")
                }
            }
        } 
        msg(paste0("Generating ", ttxfile, " ..."))
        ## -i for speed and size
        ttx(paste0("-i -t ", table, " -o ", shQuote(ttxfile),
                   " ", shQuote(fontfile)))
    }
    if (table %in% c("GlyphOrder", "hmtx", "vmtx", "glyf")) {
        rdsFile <- gsub(paste0("[.]", suffix, "$"),
                        paste0("-", replace, ".rds"), fontfile)
        ## Remove potential artifact from 'rdsFile'
        rdsFile <- gsub("-FROM-OTF", "", rdsFile)
        if (!file.exists(rdsFile)) {
            generateRDS(table, fontfile, suffix, getTTX(ttxfile), rdsFile)
        }
        getRDS(rdsFile)
    } else {
        getTTX(ttxfile)
    }
}

getHeadTable <- function(fontfile, suffix) {
    getTable("head", fontfile, suffix)
}

getHHeaTable <- function(fontfile, suffix) {
    getTable("hhea", fontfile, suffix)
}

getGlyphOrderTable <- function(fontfile, suffix) {
    getTable("GlyphOrder", fontfile, suffix)
}

getHmtxTable <- function(fontfile, suffix) {
    getTable("hmtx", fontfile, suffix)
}

getVmtxTable <- function(fontfile, suffix) {
    getTable("vmtx", fontfile, suffix)
}

getOS2Table <- function(fontfile, suffix) {
    getTable("OS/2", fontfile, suffix, "OS2")
}

getGlyfTable <- function(fontfile, suffix) {
    getTable("glyf", fontfile, suffix)
}

getNameTable <- function(fontfile, suffix) {
    getTable("name", fontfile, suffix)
}

getCMapTable <- function(fontfile, suffix) {
    getTable("cmap", fontfile, suffix)
}

################################################################################
## API functions

ttxListTables <- function(fontfile) {
    tableList(fontfile)
}

ttxGetTable <- function(table, fontfile) {
    font <- ttxFontFile(fontfile)
    switch(tolower(table),
           head=getHeadTable(font$file, font$suffix),
           hhea=getHHeaTable(font$file, font$suffix),
           glyphorder=getGlyphOrderTable(font$file, font$suffix),
           hmtx=getHmtxTable(font$file, font$suffix),
           vmtx=getVmtxTable(font$file, font$suffix),
           "os/2"=,
           os2=getOS2Table(font$file, font$suffix),
           glyf=getGlyfTable(font$file, font$suffix),
           name=getNameTable(font$file, font$suffix),
           cmap=getCMapTable(font$file, font$suffix),
           stop("Table not supported"))
}

ttxFontFamily <- function(fontfile) {
    font <- ttxFontFile(fontfile)
    nameTable <- getNameTable(font$file, font$suffix)
    gsub("^[[:space:]]+|[[:space:]]+$", "",
         xml_text(xml_find_first(nameTable, "//namerecord[@nameID = '1']")))    
}

ttxFontWeight <- function(fontfile) {
    font <- ttxFontFile(fontfile)
    OS2 <- getOS2Table(font$file, font$suffix)
    weight <- xml_text(xml_find_first(OS2, "//usWeightClass/@value"))
    if (is.na(weight))
        400
    else
        as.numeric(weight)
}

ttxFontStyle <- function(fontfile) {
    font <- ttxFontFile(fontfile)
    hhea <- getHHeaTable(font$file, font$suffix)
    slopeRun <- xml_text(xml_find_first(hhea, "//caretSlopeRun/@value"))
    if (is.na(slopeRun) || slopeRun == "0")
        "normal"
    else
        "italic"
}

ttxGlyphNames <- function(fontfile) {
    font <- ttxFontFile(fontfile)
    names(getGlyphOrderTable(font$file, font$suffix))
}

ttxGlyphIndex <- function(name, fontfile) {
    font <- ttxFontFile(fontfile)
    ## Find glyph index
    indices <- getGlyphOrderTable(font$file, font$suffix)
    ## Try more than one name (if there are multiple options)
    glyph <- NA
    while (is.na(glyph) && length(name)) {
        glyph <- indices[name[1]]
        name <- name[-1]
    }
    glyph
}

## Convert 4-digit hex code to glyph name via font Unicode mapping
ttxGlyphNameFromUNICODE <- function(code, file, dir) {
    font <- ttxFontFile(file)
    cmap <- getCMapTable(font$file, font$suffix)
    ## cmap table with platformID="0" is UNICODE mapping
    ## NOTE: this may need relaxing to allow for other table formats
    ##       (e.g., cmap_format_6)
    unicodeMap <- paste0("//cmap_format_4[@platformID = '0']/map[@code = '",
                         code, "']")
    name <- xml_attr(xml_find_first(cmap, unicodeMap), "name")
    if (length(name)) {
        if (dir == 1) {
            ## Prefer .vert variation if it exists
            c(paste0(name, ".vert"), name)
        } else {
            name
        }
    } else {
        NULL
    }   
}

ttxGlyphWidth <- function(index, fontfile) {
    font <- ttxFontFile(fontfile)
    headTTX <- getHeadTable(font$file, font$suffix)
    unitsPerEm <- as.numeric(xml_text(xml_find_first(headTTX,
                                                     "//unitsPerEm/@value")))
    hmtx <- getHmtxTable(font$file, font$suffix)
    ## '+ 1' because glyph ids are zero-based in XML
    ## BUT I want to use them to index a (one-based) R vector or matrix
    width <- hmtx[index + 1, ]
    attr(width, "unitsPerEm") <- unitsPerEm
    width
}

ttxGlyphHeight <- function(index, fontfile) {
    font <- ttxFontFile(fontfile)
    headTTX <- getHeadTable(font$file, font$suffix)
    unitsPerEm <- as.numeric(xml_text(xml_find_first(headTTX,
                                                     "//unitsPerEm/@value")))
    vmtx <- getVmtxTable(font$file, font$suffix)
    ## '+ 1' because glyph ids are zero-based in XML
    ## BUT I want to use them to index a (one-based) R vector or matrix
    height <- vmtx[index + 1, ]
    attr(height, "unitsPerEm") <- unitsPerEm
    height
}

ttxGlyphBounds <- function(index, fontfile) {
    font <- ttxFontFile(fontfile)
    head <- getHeadTable(font$file, font$suffix)
    unitsPerEm <- as.numeric(xml_text(xml_find_first(head,
                                                     "//unitsPerEm/@value")))
    glyf <- getGlyfTable(font$file, font$suffix)
    ## '+ 1' because glyph ids are zero-based in XML
    ## BUT I want to use them to index a (one-based) R vector or matrix
    bbox <- glyf[index + 1, ]
    attr(bbox, "unitsPerEm") <- unitsPerEm
    bbox
}

