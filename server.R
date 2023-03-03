library(shiny)
library(dplyr)
library(leaflet)
library(data.table)
library(DT)
library(htmlwidgets)

shinyServer(function(input, output) {
  
  # Import Data and clean it
  load('01-data_download/03dadesWithROI.RData')
  
  #grouping for plotting
  value_labels<-c(-Inf,6,8,9,10,12,Inf)
  color_labels<-c('Inferior al 6%','6-8%','8-9%','9-10%','10-12%','Superior al 12%')
  
  dadesSale[,rentabilidad_grupo:= as.character(cut(ROIpct,value_labels, labels=color_labels))] #important que sigui character
  
  #filtering some wrong ones
  lowers10<-(!dadesSale$rentabilidad_grupo %in% c('10-12%'))
  lowers12<-(!dadesSale$rentabilidad_grupo %in% c('Superior al 12%'))
  dadesSale<-dadesSale[c(which(lowers10 & lowers12),which(!lowers10)[1:25], which(!lowers12)[1:25]),]
  
  # create a color palette for category type in the data file
  colors_vec<-c('#FF625E','#FFBE65','#F5EEA5', '#D2FDBB', '#77DD76' ,'#0CC078')
  pal <- colorFactor(palette = colors_vec,
                     levels = color_labels)
  
  #filter data appearing on the map
  map_dadesSale_react <- reactive({
    
    dadesSale %>% 
      filter(price >= min(input$priceMinSelectInput,0,na.rm=T) &
                           price <= min(input$priceMaxSelectInput,Inf,na.rm=T) &
                           SuggestedRentalPrice >= min(input$rentMinSelectInput,0,na.rm=T) &
                           SuggestedRentalPrice <= min(input$rentMaxSelectInput,Inf,na.rm=T)) %>% 
      filter(if(sum(unlist(input$statusSelectInput) != '')==0) TRUE else (status %in% input$statusSelectInput)) %>% #no check, no filter
      filter(if(sum(unlist(input$roiSelectInput) != '')==0) TRUE else (rentabilidad_grupo %in% input$roiSelectInput))
    
  })
  
  # leaflet Sale Data map
  output$saleMap <- renderLeaflet({
    
    dadesSaleLeaflet <- map_dadesSale_react()
    
      leaflet(dadesSaleLeaflet) %>% 
      addCircles(lng = ~longitude, lat = ~latitude) %>% 
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
      addCircleMarkers(data=dadesSaleLeaflet,
                       lat =  ~latitude, lng =~longitude, layerId = ~propertyCode,
                       radius = 8, popup = ~as.character(cntnt), 
                       fillColor = ~pal(rentabilidad_grupo),
                       stroke = T, color = "black", opacity=0.25, fillOpacity = 1)%>%
      addLegend(colors=colors_vec,labels=color_labels, values= ~rentabilidad_grupo,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      setView(lng = 1.747969, lat = 41.912181, zoom=8) %>%
        onRender("
                     function(el, x) {
                         this.on('popupopen', function(e) {
                             Shiny.onInputChange('myEvent', 'open');
                         });

                         this.on('popupclose', function(e) {
                             Shiny.onInputChange('myEvent', 'close');
                         });
                     }")
        
        })
  
  #display Filtered Referenced Rent data when user selects a property for sale
  
  observeEvent(input$saleMap_marker_click, {
    
    selectedProperty <- input$saleMap_marker_click$id
    selectedLatitude <- input$saleMap_marker_click$lat
    selectedLongitude <- input$saleMap_marker_click$lng
    
    referencedProperties<- (dadesSale %>% 
                              filter(propertyCode == selectedProperty) %>% 
                              select(ReferencedRentals))[[1]][[1]] %>% lapply('[[',1) %>% unlist
    
    filteredRentData<-dadesRent  %>% filter(propertyCode %in% referencedProperties)
    
    
    filteredRentDataOutput<-datatable(filteredRentData[,c('municipality','price','priceByArea',
                                                                'propertyType','size',
                                                                'rooms','floor','hasLift',
                                                                'exterior','status','url_html')],
                                      filter = 'none',
                                      colnames = c("Localidad","Alquiler", "Alquiler/m2",
                                                   "Tipo", "m2",
                                                   "Habitaciones","Altura","Ascensor",
                                                   "Exterior","Estado", "Enlace"),
                                      options=list(pageLength=-1,
                                                   dom='tr',
                                                   scrollX=T),
                                      escape=FALSE,
                                      caption = htmltools::tags$caption("Pisos de alquiler parecidos con los que se calcula el alquiler estimado y su rentabilidad asociada.", 
                                                                        style="text-align: center; font-weight: bold; color:black"),
                                      selection = 'none')
    #table
    output$filteredRentData <-
      DT::renderDataTable(filteredRentDataOutput)
    
    #map
    # leaflet filtered Rent Data map
    output$rentMap <- renderLeaflet({

      leaflet(filteredRentData) %>% 
        addCircles(lng = ~longitude, lat = ~latitude) %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
        addCircleMarkers(data=filteredRentData,
                         lat =  ~latitude, lng =~longitude, layerId = ~propertyCode,
                         radius = 8, popup = ~as.character(cntnt), 
                         fillColor = "#77ccff",
                         stroke = T, color = "black", opacity=0.7, fillOpacity = 1) %>%

        addEasyButton(easyButton(
          icon="fa-crosshairs", title="ME",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
        setView(lng = selectedLongitude, lat = selectedLatitude, zoom=10)
      
    })
    
  })

  #####close referenced properties table/map
  
  #get the click eventwhen the user clicks on the map (we want to close the reference properties map/table)
  observeEvent(input$saleMap_click, {

    output$filteredRentData<-NULL
    output$rentMap<-NULL

  })
  
  #get the click event when the user clicks on the close popup (we want to close the reference properties map/table)
  observeEvent(input$myEvent, {
    
    output$filteredRentData<-NULL
    output$rentMap<-NULL
  })
  
  
  
  
  
  
  
  ##### PESTANYA 2
  
  #display Sale data
  saleDataOutput<-datatable(dadesSale[,c('ROIpct','price','priceByArea',
                                                        'SuggestedRentalPrice','propertyType','size',
                                                        'rooms','floor','hasLift',
                                                        'exterior','status','address','url')],
                                    filter = 'top',
                                    colnames = c("Rentabilidad", "Precio", "Precio/m2",
                                                 "Alquiler estimado", "Tipo", "m2",
                                                 "Habitaciones","Altura","Ascensor",
                                                 "Exterior","Estado", "Dirección", "Enlace"),
                            options=list(pageLength=20,
                                         lengthMenu=c(10,20,50),
                                         dom='lptrp'),
                            selection = 'none') 
  output$saleData <-
    DT::renderDataTable(saleDataOutput)
  
  ##### PESTANYA 3
  
  #display Rent data
  
  output$rentData <-DT::renderDataTable(datatable(
    dadesRent[,c('price','priceByArea','propertyType','size','rooms','floor','hasLift','exterior',
                 'status','address','url')],filter = 'top',
    colnames = c("Alquiler", "Alquiler/m2", "Tipo", "m2","Habitaciones",
                 "Altura","Ascensor","Exterior","Estado", "Dirección", "Enlace"),
    options=list(pageLength=20,
                 lengthMenu=c(10,20,50),
                 dom='lptrp'),
    selection = 'none'))
  
  
  
})
