
library(ttx)

font <- system.file("fonts", "Montserrat", "static", "Montserrat-Medium.ttf",
                    package="grDevices")

ttxListTables(font)

ttxGetTable("head", font)

ttxFontFamily(font)
ttxFontWeight(font)
ttxFontStyle(font)
       
head(ttxGlyphNames(font))
ttxGlyphIndex("A", font)
ttxGlyphWidth(1, font)
ttxGlyphHeight(1, font) ## NOTE heights for vertical writing
ttxGlyphBounds(1, font)




