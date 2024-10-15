
library(ttx)

font <- system.file("fonts", "Montserrat", "static", "Montserrat-Medium.ttf",
                    package="grDevices")

suppressMessages(ttxListTables(font))

suppressMessages(ttxGetTable("head", font))

suppressMessages(ttxFontFamily(font))
suppressMessages(ttxFontWeight(font))
suppressMessages(ttxFontStyle(font))
       
suppressMessages(head(ttxGlyphNames(font)))
suppressMessages(ttxGlyphIndex("A", font))
suppressMessages(ttxGlyphWidth(1, font))
suppressMessages(ttxGlyphHeight(1, font)) ## NOTE heights for vertical writing
suppressMessages(ttxGlyphBounds(1, font))




