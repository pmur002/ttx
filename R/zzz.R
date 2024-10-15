
.onLoad <- function(libname, pkgname) {
    initTTX()
    initFontForge()
    options(ttx.quiet=TRUE)
}

