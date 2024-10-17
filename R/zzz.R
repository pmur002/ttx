
.onLoad <- function(libname, pkgname) {
    initTTX()
    initFontForge()
    options(ttx.quiet=TRUE)
}

.onAttach <- function(libname, pkgname) {
    if (ttxAvailable()) {
        packageStartupMessage(paste0("      ttx:  ", ttxVersion()))
    } else {
        packageStartupMessage(paste("         :  ttx not found",
                                    "(this package will not work until ttx is installed)"))
    }
    if (fontForgeAvailable()) {
        packageStartupMessage(paste0("fontforge:  ", fontForgeVersion()))
    } else {
        packageStartupMessage(paste("         :  fontforge not found",
                                    "(no support for OTF fonts that lack glyf table)"))
    }
}
