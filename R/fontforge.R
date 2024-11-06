
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
    quiet <- getOption("ttx.quiet")
    if (!quiet) {
        Sys.setenv("FONTFORGE_VERBOSE"="true")
    }
    system(paste0("fontforge ",
                  "-lang=ff ",
                  "-script ",
                  system.file("script", "otf2ttf", package="ttx"),
                  " ", shQuote(otfFile), " ", shQuote(ttfFile)),
           ignore.stderr=quiet, ignore.stdout=quiet)
    if (!quiet) {
        Sys.unsetenv("FONTFORGE_VERBOSE")
    }
}
