# load the necessary packages
# install.packages("hexSticker")
# install.packages("magick")
# install.packages("sysfonts")
# install.packages("tidyverse")

library(hexSticker)             # hexSticker generator
library(magick)                 # Advanced image processing
library(sysfonts)               # font selection
library(tidyverse)              # data manipulation

# Sticker function---------------------------

# Create your first sticker------------------
InvGator <- image_read('Investogator_Icon.png')

fonts_dataset <- font_files()

sticker(
  subplot = InvGator,          # image/ggplot object
  package = "Investigator",    # package name to be displayed in hexSticker
  s_width = 1,                 # subplot width
  s_height = 1,                # subplot height
  s_x = 1,                     # subplot x-position
  s_y = 0.75,                  # subplot y-position
  p_size = 15,                 # package name font size
  h_fill = "#0a212f",          # hexSticker background color
  h_color = "#537286",         # hexsticker border color
  h_size = 1.5,                # hexSticker size
  spotlight = T,               # add spotlight effect to hexSticker
  l_y = 1,                     # spotlight effect y-position
  l_x = 1,                     # spotlight effect x-position
  l_width = 3,                 # spotlight effect width
  l_height = 3,                # spotlight effect height
  l_alpha = 0.3,               # spotlight effect level
  p_color = 'white',           # package name color
) %>% print()

