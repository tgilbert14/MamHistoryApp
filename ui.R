##NEON Mammal History App##
#Will look into NEON database and find tagID if it exists on the selected site in the app
#Will print out what tag selected, history of captures on NEON database, and plot when it
#was caught (collection date) vs where on the plot it was captured (trap coordinate)
#If MAM history and plot has no info = could not find that tagID at selected site on NEON database
#Must enter domain # and tagID in correct format, leading w/ domain followed by tag
#identification number [D##.tagID#] {Ex: 'club foot' at JORN - R2861}
load.pkg <- function(p) {  #load packages with require(), install any that are not installed
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  suppressMessages(require(p, character.only = TRUE))
}

load.pkg('neonUtilities')  ##Loading Libraries##
load.pkg('dplyr')
load.pkg('tidyverse')
load.pkg('readr')
load.pkg('tidyr')
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
load.pkg('htmlwidgets')
load.pkg('webshot')

#---user interface--------------------------------------------------  


ui <- fluidPage(
  titlePanel("NEON Mammal History App"),
  sidebarLayout(
    sidebarPanel(
      #selection area for each site- can type in site and app will find from list below
      selectInput("Select", "Please select your site:",
                  choices = c("SRER", "JORN", "BART", "HARV", "BLAN", "SCBI", "SERC", "DSNY", "JERC", "OSBS", "GUAN", "LAJA","STEI", "TREE", "UNDE","KONA", "KONZ", "UKFS", "GRSM", "MLBS", "ORNL","DELA", "LENO", "TALL", "DCFS", "NOGP", "WOOD", "CPER", "RMNP", "STER", "CLBJ", "OAES", "YELL", "MOAB", "NIWO", "JORN", "SRER", "ONAQ", "ABBY", "WREF", "SJER", "SOAP", "TEAK", "BARR", "TOOL", "BONA", "DEJU", "HEAL"), selected = F, multiple = T),
     
      selectInput('view', "Select map view:",
                  choices = c("Esri.WorldImagery","TomTom.Hybrid","Stamen.TopOSMRelief")),
      submitButton("Process Selection", icon('globe-americas'), width = '200px'), 
    #  checkboxInput('yes', 'download map?', FALSE), 
     # conditionalPanel(
      #  condition = "input.yes == true",
      #  downloadButton('map_down')
    #  ),
      #type in tagID looking for and will save as variable
      textInput("SelectID", "Type in tag of individual species:", value= "", placeholder = 'R2094'),
      
      
      submitButton("Process Tag", icon('tag'), width = '200px'),
    ),
    
    mainPanel("Please wait for app to access NEON data portal",
              tabsetPanel(
                
                tabPanel("Map View of Capture by Species",
                         withSpinner(leafletOutput("map", width = '100%', height = 700))),
                         #downloadButton('downloadmap', label = "Save Map")),
                
                tabPanel("Top Captured Individuals",
                         withSpinner(dataTableOutput("FitSelect"))),
                
                tabPanel("Captures Per Plot",
                         withSpinner(plotlyOutput("DataSet1"))),
                
                tabPanel("MAM Capture Histroy",
                         withSpinner(dataTableOutput("NEONDataSet1"))),
                
                tabPanel("Raw Data",
                         withSpinner(dataTableOutput("data1"))),
                
                tabPanel("Trap Coordinate Heat Map",
                         withSpinner(plotlyOutput("plot2"))),
                
                tabPanel("Experimental Heat Map View",
                         withSpinner(plotlyOutput("plot3")))
              )
    )
  )
)
