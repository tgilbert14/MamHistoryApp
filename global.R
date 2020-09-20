load.pkg <- function(p) {  #load packages with require(), install any that are not installed
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  suppressMessages(require(p, character.only = TRUE))
}

load.pkg('neonUtilities')  ##Loading Libraries##
load.pkg('tidyverse')
load.pkg('readr')
#load.pkg('tidyr')
load.pkg('plotly')
load.pkg('shiny')
load.pkg('shinycssloaders')
load.pkg('reshape')
load.pkg('RColorBrewer')
load.pkg('grid')
load.pkg('leaflet')
load.pkg('ggmap')
load.pkg('sf')
load.pkg('mapview')
load.pkg('plotly')
#load.pkg('dplyr')
load.pkg('htmlwidgets')
load.pkg('webshot')