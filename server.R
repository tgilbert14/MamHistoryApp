#--Server----------------------------------------------------------------
server <- function(input, output, session) {
  
  site_select <- reactive({
    site<- input$Select #saving site selection
    #site<- input$Select
    if (is.null(input$Select))
      return(NULL)
    
    #Downloading NEON portal data since 2016 to present w/ dpID
    raw <- loadByProduct(dpID = "DP1.10072.001", site = site, startdate = '2016-01', package = 'basic', check.size = 'F' )
    data.raw <- as_tibble(raw$mam_pertrapnight)    #Getting raw data
  })
  
  view_select <- reactive({
    view_pick<- input$view
                
  })
  
 # dl_select <- reactive({
#    dl_pick<- input$yes
 #   
#  })
  
  ID_select <- reactive({
    ID.raw<- input$SelectID #saving tagID selection
    site<- input$Select #saving site selection
    
    if (is.null(site))
      return(NULL)
    
    d1<- (paste0("D0",c(1:9)))
    d2<- (paste0("D",c(10:20)))
    domain<- c(d1,d2)
    
    if(site == 'HARV' | site == 'BART') {ID<- paste0("NEON.MAM.",domain[1],".",ID.raw)}
    if(site == 'SCBI' | site == 'SERC' | site == 'BLAN') {ID<- paste0("NEON.MAM.",domain[2],".",ID.raw)}
    if(site == 'OSBS' | site == 'DSNY' | site == 'JERC') {ID<- paste0("NEON.MAM.",domain[3],".",ID.raw)}
    if(site == 'GUAN' | site == 'LAJA') {ID<- paste0("NEON.MAM.",domain[4],".",ID.raw)}
    if(site == 'UNDE' | site == 'STEI' | site == 'TREE') {ID<- paste0("NEON.MAM.",domain[5],".",ID.raw)}
    if(site == 'KONZ' | site == 'UKFS' | site == 'KONA') {ID<- paste0("NEON.MAM.",domain[6],".",ID.raw)}
    if(site == 'GRSM' | site == 'ORNL' | site == 'MLBS') {ID<- paste0("NEON.MAM.",domain[7],".",ID.raw)}
    if(site == 'TALL' | site == 'LENO' | site == 'DELA') {ID<- paste0("NEON.MAM.",domain[8],".",ID.raw)}
    if(site == 'WOOD' | site == 'DCFS' | site == 'NOGP') {ID<- paste0("NEON.MAM.",domain[9],".",ID.raw)}
    if(site == 'STER' | site == 'CPER' | site == 'RMNP') {ID<- paste0("NEON.MAM.",domain[10],".",ID.raw)}
    if(site == 'CLBJ' | site == 'OAES') {ID<- paste0("NEON.MAM.",domain[11],".",ID.raw)}
    if(site == 'YELL') {ID<- paste0("NEON.MAM.",domain[12],".",ID.raw)}
    if(site == 'NIWO' | site == 'MOAB') {ID<- paste0("NEON.MAM.",domain[13],".",ID.raw)}
    if(site == 'JORN' | site == 'SRER') {ID<- paste0("NEON.MAM.",domain[14],".",ID.raw)}
    if(site == 'ONAQ') {ID<- paste0("NEON.MAM.",domain[15],".",ID.raw)}
    if(site == 'WREF' | site == 'ABBY') {ID<- paste0("NEON.MAM.",domain[16],".",ID.raw)}
    if(site == 'SJER' | site == 'SOAP' | site == 'TEAK') {ID<- paste0("NEON.MAM.",domain[17],".",ID.raw)}
    if(site == 'TOOL' | site == 'BARR') {ID<- paste0("NEON.MAM.",domain[18],".",ID.raw)}
    if(site == 'BONA' | site == 'DEJU' | site == 'HEAL') {ID<- paste0("NEON.MAM.",domain[19],".",ID.raw)}
    if(site == 'OLAA') {ID<- paste0("NEON.MAM.",domain[20],".",ID.raw)}
    ID
  })
  
  
  output$FitSelect <- renderDataTable({    
    data.raw <- site_select()
    
    if (is.null(data.raw))
      return(NULL)
    
    fit<- data.raw %>%
      group_by(tagID, taxonID, plotID) %>% 
      summarise(count = n()) %>% 
      filter(!is.na(tagID)) %>% 
      arrange(desc(count))
    
    fit$tagID<- substr(fit$tagID,14,50)
    colnames(fit)[4]<- '# times capture'
    
    fit
  })
  
  mymap <- reactive({
    # here I have specified a tile from openstreetmap
    data.raw <- site_select()
    view_pick<- view_select()
    
    if (is.null(data.raw))
      return(NULL)
    
    ###Make code to look at sites lat/long and choose a focus point! Only works for SRER for now...
    
    data<- data.raw %>% 
      filter(!is.na(tagID)) %>% 
      select(plotID, decimalLongitude,decimalLatitude)
    colnames(data)[2]<- 'Longitude'
    colnames(data)[3]<- 'Latitude'
    
    ##For total # of species captured at each plot
    per<- data.raw %>% 
      group_by(taxonID, collectDate) %>% 
      filter(!is.na(taxonID))
    
    per$year <-  str_sub(per$collectDate, 1, 4) 
    
    all_species<- per %>% 
      group_by(year) %>% 
      summarise(Totalcount=n())
    
    y2016<- all_species %>% 
      filter(year == '2016')
    y2017<- all_species %>% 
      filter(year == '2017')
    y2018<- all_species %>% 
      filter(year == '2018')
    y2019<- all_species %>% 
      filter(year == '2019')
    y2020<- all_species %>% 
      filter(year == '2020')
    #y2019
    ##Not using as of yet
    per_sp<- data.raw %>% 
      select(scientificName, plotID, collectDate) %>% 
      group_by(plotID, scientificName) %>% 
      summarise(count=n()) %>% 
      filter(!is.na(scientificName))
    
    geo_per_plot <- left_join(per_sp, data, by = 'plotID')
    
    new_geo<- unique(geo_per_plot)
    
    #For setting map view center
    coord<- new_geo %>% 
      mutate(meanLon = mean(Longitude)) %>% 
      mutate(meanLat = mean(Latitude))
    
    nb.cols <- length(new_geo$scientificName)
    mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
    
    pal <- colorFactor(
      palette = mycolors,
      domain = new_geo$scientificName)
    
    # Prepare the text for the tooltip:
    mytext2 <- paste(
      "ScientificName: ", new_geo$scientificName, "<br/>", 
      "Count: ", new_geo$count, "<br/>",
      "PlotID: ", new_geo$plotID, "<br/>", 
      "Long: ", new_geo$Longitude,"<br/>",
      "Lat: ", new_geo$Latitude, sep="") %>%
      lapply(htmltools::HTML)
    
    m <- leaflet(new_geo) %>% 
      addTiles()  %>% 
      setView( lat=coord$meanLat[1], lng=coord$meanLon[1], zoom=11) %>%
      addProviderTiles(view_pick) %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       fillColor =~pal(new_geo$scientificName), opacity = .8, fillOpacity = .8, radius=~count/8,
                       popup = paste("Total Captures at site (2016): ",y2016$Totalcount,"<br/>","Total Captures at site (2017): ",y2017$Totalcount,"<br/>",'Total Captures at site (2018): ',y2018$Totalcount,"<br/>",'Total Captures at site (2019): ',y2019$Totalcount,"<br/>",'Total Captures at site (2020): ',y2020$Totalcount, sep="") %>%
                         lapply(htmltools::HTML),
                       stroke = T, weight = 1,  color = 'white', 
                       label = mytext2,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>% 
      addLegend( pal=pal, values=~new_geo$scientificName, opacity=0.9, title = "Species Diversity Per Plot", position = "bottomright" )
    
    #dl_pick<- dl_select()
    #if (dl_pick == T) {
    #  saveWidget(m, file="MAM_captures.html")
    #}
      m 
    
  })
  
  output$map <- renderLeaflet({   
    mymap()
  })
  
  # function with all the features that we want to add to the map
#  myfun <- function(map){
#    addCircles(map,12.5,42,radius=500) %>% addMarkers(12,42,popup="Rome")
#  }
  
#  observe({
#    leafletProxy("map") %>% myfun()
#  })
  
  # map that will be downloaded
#  mapdown <- reactive({
    # we need to specify coordinates (and zoom level) that we are currently viewing
#    bounds <- input$map_bounds
#    latRng <- range(bounds$north, bounds$south)
#    lngRng <- range(bounds$east, bounds$west)
 #   mymap() %>% myfun() %>% setView(lng = (lngRng[1]+lngRng[2])/2, lat = (latRng[1]+latRng[2])/2, zoom = input$map_zoom)
  #})
  
#  output$map_down <- downloadHandler(
 #   filename = 'mymap.pdf',
    
#    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
 #     owd <- setwd(tempdir())
#      on.exit(setwd(owd))
      
      # using saveWidget and webshot (old)
 #   saveWidget(mapdown(), "temp.html", selfcontained = FALSE)
#      webshot("temp.html", file = file, cliprect = "viewport")
 #   }
#  )
      
  output$DataSet1 <- renderPlotly({    
    data.raw <- site_select()
    
    if (is.null(data.raw))
      return(NULL)
    
    #names(data.raw)
    data.raw$year_month <-  str_sub(data.raw$collectDate, 1, 7)
    #View(data.raw)
    data.sum<- data.raw %>%
      group_by(plotID, scientificName, year_month) %>% 
      summarise(count = n()) %>% 
      filter(!is.na(scientificName)) %>% 
      arrange(desc(count))
    
    col_num<- data.sum %>% 
      group_by(scientificName) %>% 
      count()
    
    nb.cols <- nrow(col_num)
    mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
    
    
    #View(data.sum)
    a.plot<- ggplot(data.sum, aes(year_month, count, group = (scientificName), fill = (scientificName)))+
      geom_point(aes(color=scientificName), size=.7)+
      geom_line(aes(color=scientificName)) +
      facet_wrap(facets = vars(plotID), scales = 'free')+
      scale_fill_manual(values = mycolors) +
      ggtitle(paste0(data.raw$siteID[1]," Species Per Plot by Month"))+
      #scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.19)))+
      xlab("Date (Year_month)")+
      ylab("Species Count")+
      theme(axis.text.x = element_text(size = 8, angle = 45))
    
    #mtext("Date (Year_month)", side=1, line=5)
    
    #mtext("Date (Year_month)", side=3)
    
    a.plot <- a.plot + theme(panel.spacing.x=unit(1, "lines"),panel.spacing.y=unit(1.5, "lines"))
    a.plot<- ggplotly(a.plot, width = 1500, height = 800)
    a.plot
    
  })
  
  output$NEONDataSet1 <- renderDataTable({    
    #pulling data from NEON
    data.raw <- site_select()
    ID<- ID_select()
    
    if (is.null(ID))
      return(NULL) 
    if (is.null(data.raw))
      return(NULL)
    
    tagID<- data.raw %>% 
      filter(tagID == ID) %>% 
      select(tagID, plotID, collectDate, recapture, scientificName, identificationQualifier, hindfootLength, weight, lifeStage, sex, testes, nipples, pregnancyStatus, vagina, fate, remarks)
    
    tagID$collectDate<- substr(tagID$collectDate,1,10)
    tagID
  })
  
  #---ploting each species HF vs W to look for outliers- prints from current data
  output$data1 <- renderDataTable({
    #pulling data from NEON
    data.raw <- site_select()
    ID<- ID_select()
    
    if (is.null(ID))
      return(NULL) 
    if (is.null(data.raw))
      return(NULL) 
    
    tagID_h<- data.raw %>% 
      filter(tagID == ID)
    
    #All data
    tagID_h
    
  })
  
  
  output$plot2 <- renderPlotly({
    data.raw <- site_select()
    ID<- ID_select()
    site<- site_select()
    
    if (is.null(ID))
      return(NULL) 
    if (is.null(data.raw))
      return(NULL) 
    
    ###Testing variables
    #      site<- 'JORN'
    #      raw <- loadByProduct(dpID = "DP1.10072.001", site = site, startdate = '2017-01', package = 'basic', check.size = 'F' )
    #      data.raw <- as_tibble(raw$mam_pertrapnight)    #Getting raw data
    #     ID<- 'NEON.MAM.D14.R2861'
    #--------------------------------------------------
    data1<- data.raw %>% 
      filter(tagID == ID)%>%
      group_by(trapCoordinate,plotID,tagID) %>%
      summarise(count = n())
    
    y<- matrix(data=0, nrow = 10,ncol = 10)
    colnames(y)<- c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K")
    num<- c(1:10)
    
    x=1
    while(x<12){
      i=1
      while (i<11) {
        data2<- data1%>%
          filter(trapCoordinate == paste0(LETTERS[x],num[i]))
        if (nrow(data2)>0){
          y[i,x]<- data2$count
        }
        i=i+1
      }
      x=x+1
    }
    
    data_melt<- melt(y)
    data_melt<- data_melt %>% 
      mutate(captures = value)
    #View(data_melt)
    #View(data1)
    ggp<-ggplot(data_melt, aes(X2,X1))+
      geom_tile(aes(fill=captures))+
      scale_y_discrete('Trap Number', limits= factor(c(1:10)))+
      scale_x_discrete('Trap Letter', position= 'top')+
      scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.19)))+
      ggtitle(paste0("Capture Heat Map at ",data1$plotID[1]))
    ggplotly(ggp, text =~paste0("Captures: ",data_melt$captures, '\n',"PlotID: ",data1$plotID, '\n', "TrapCoordinate: ",data1$trapCoordinate), hoverinfo='text')
    ##Custom hover text does not work for now
  })
  
  output$plot3 <- renderPlotly({
    data.raw <- site_select()
    ID<- ID_select()
    
    if (is.null(ID))
      return(NULL) 
    if (is.null(data.raw))
      return(NULL)
    
    data1<- data.raw %>% 
      filter(tagID == ID)%>%
      group_by(trapCoordinate,plotID,tagID) %>%
      summarise(count = n())
    
    y<- matrix(data=0, nrow = 10,ncol = 10)
    colnames(y)<- c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K")
    num<- c(1:10)
    
    x=1
    while(x<12){
      i=1
      while (i<11) {
        data2<- data1%>%
          filter(trapCoordinate == paste0(LETTERS[x],num[i]))
        if (nrow(data2)>0){
          y[i,x]<- data2$count
        }
        i=i+1
      }
      x=x+1
    }
    #rm(y2)
    y2<- exp(y)*2
    #View(y2)      
    x=1
    while (x < 10) {
      i=1
      while(i < 10) {
        if(y2[i,x] >= 5) {
          y2[i,x] <- y2[i,x]+3
          y2[i-1,x]<- y2[i-1,x]+1.1
          y2[i+1,x]<- y2[i+1,x]+1.1
          y2[i,1+x]<- y2[i,1+x]+1.1
          y2[i,1-x]<- y2[i,1-x]+1.1
          
        }
        i=i+1
      }
      x=x+1 
    }
    
    x=1
    while (x < 10) {
      i=1
      while(i < 10) {
        if(y2[i,x] >= 14) {
          y2[i,x] <- y2[i,x]+6
          y2[i-1,x]<- y2[i-1,x]+1.8
          y2[i+1,x]<- y2[i+1,x]+1.8
          y2[i,1+x]<- y2[i,1+x]+1.8
          y2[i,1-x]<- y2[i,1-x]+1.8
          
        }
        i=i+1
      }
      x=x+1 
    }     
    
    x=1
    while (x < 10) {
      i=1
      while(i < 10) {
        if(y2[i,x] >= 36) {
          y2[i,x] <- y2[i,x]+9
          y2[i-1,x]<- y2[i-1,x]+2.5
          y2[i+1,x]<- y2[i+1,x]+2.5
          y2[i,1+x]<- y2[i,1+x]+2.5
          y2[i,1-x]<- y2[i,1-x]+2.5
          
        }
        i=i+1
      }
      x=x+1 
    }     
    #----------
    #View(y2)
    data_melt2<- melt(y2)
    data_melt2<- data_melt2 %>% 
      mutate(hot_spots = value)
    
    ggp2<-ggplot(data_melt2, aes(X2,X1))+
      geom_tile (aes(fill=hot_spots))
    ggp2<- ggp2+scale_y_discrete('Trap Number', limits= factor(c(1:10)))
    ggp2<- ggp2+scale_x_discrete('Trap Letter')
    ggp2<- ggp2+scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.15)))+
      ggtitle(paste0("Hot Spot Heat Map at ", data1$plotID[1]))
    ggplotly(ggp2)
    
    
  })
}


