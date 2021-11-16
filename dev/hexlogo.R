#testing out a new hexlogo
library(hexSticker)
library(magrittr)
library(showtext)
font_add_google("Inter")
 showtext_auto()
file.path("./vignettes/figures/aqslogotransparent.png") %>%
  sticker(AQSlogo, package="RAQSAPI", p_size=14, s_x=1, s_y=0.70, s_width=.5,
          p_family = "Inter", h_fill="coral1" , filename="vignettes/figures/RAQSAPIhexsticker.png",
          url="https://cran.r-project.org/package=RAQSAPI", white_around_sticker=FALSE) %>%
  print
