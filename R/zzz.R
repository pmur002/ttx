
.onLoad <- function(libname, pkgname) {
    initTTX()
    initFontForge()
    options(ttx.quiet=TRUE)
}

.onAttach <- function(libname, pkgname) {
    if (ttxAvailable()) {
        packageStartupMessage(paste0("      ttx:  ", ttxVersion()))
    } else {
        packageStartupMessage("      ttx:  not found")
        packageStartupMessage(paste("         : ",
                                    "This package will not work until ttx is installed"))
    }
    if (fontForgeAvailable()) {
        packageStartupMessage(paste0("fontforge:  ", fontForgeVersion()))
    } else {
        packageStartupMessage("fontforge:  not found")
        packageStartupMessage(paste("         : ",
                                    "No support for OTF fonts that lack glyf table"))
    }
}
