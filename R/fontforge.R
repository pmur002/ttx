
initFontForge <- function() {
    fontforge <- Sys.which("fontforge")
    if (nchar(fontforge) > 0) {
        versText <- system("fontforge --version", intern=TRUE,
                           ignore.stderr=TRUE)
        versLine <- grep("fontforge", versText)
        version <- gsub(".+ ", "", versText[versLine])
        set("ffVersion", version)
    }
}

fontForgeVersion <- function() {
    get("ffVersion")
}

fontForgeAvailable <- function() {
    !is.null(fontForgeVersion())
}

otf2ttf <- function(otfFile, ttfFile) {
    msg(paste0("Converting ", otfFile, " to ", ttfFile))
    system(paste0("fontforge -lang=ff -c 'Open($1); Reencode(\"original\"); Generate($2); Close();' ",
                  otfFile, " ", ttfFile),
           ignore.stderr=TRUE,
           ignore.stdout=getOption("ttx.quiet"))
}
