library(shiny)

library(dplyr)

library(leaflet)

library(data.table)

library(DT)

shinyServer(function(input, output) {
  
  # Import Data and clean it
  load('01-data_download/03dadesWithROI.RData')
  
  dadesSale[,ROIpct:= round(ROIpct,1)]
  dadesSale[,SuggestedRentalPrice:= round(SuggestedRentalPrice)]
    
  value_labels<-c(-Inf,6,8,9,10,12,Inf)
  color_labels<-c('Inferior al 6%','6-8%','8-9%','9-10%','10-12%','Superior al 12%')
  
  dadesSale[,rentabilidad_grupo:= as.character(cut(ROIpct,value_labels, labels=color_labels))] #important que sigui character
  
  lowers10<-(!dadesSale$rentabilidad_grupo %in% c('10-12%'))
  lowers12<-(!dadesSale$rentabilidad_grupo %in% c('Superior al 12%'))
  dadesSale<-dadesSale[c(which(lowers10 & lowers12),which(!lowers10)[1:25], which(!lowers12)[1:25]),]
  
  # new column for the popup label
  
  dadesSale <- mutate(dadesSale, cntnt=paste0('<strong>Rentabilidad: </strong>',paste(ROIpct,'%',sep=''),
                                              '<br><strong>Precio:</strong> ', price,
                                              '<br><strong>Precio por m2:</strong> ', priceByArea,
                                              '<br><strong>Alquiler estimado:</strong> ', SuggestedRentalPrice,
                                              '<br><strong>Tipo de propiedad:</strong> ',propertyType,
                                              '<br><strong>m2:</strong> ',size,
                                              '<br><strong>Habitaciones:</strong> ',rooms,
                                              '<br><strong>Altura:</strong> ',floor,
                                              '<br><strong>Ascensor:</strong> ',hasLift,
                                              '<br><strong>Exterior:</strong> ',exterior,
                                              '<br><strong>Estado:</strong> ',status,
                                              '<br><strong>Dirección:</strong> ',address,
                                              '<br><strong>Enlace:</strong> ',url))
  
  
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
  output$bbmap <- renderLeaflet({
    
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
      setView(lng = 1.747969, lat = 41.912181, zoom=8)
        })
  
  
 
  
  #get the click event from the map (when the user clicks a property)
  observeEvent(input$bbmap_marker_click, { # update the location selectInput on map clicks
    selectedProperty <- input$bbmap_marker_click$id
    print(selectedProperty)
    filteredSaleData<-dadesSale[,] %>% filter(propertyCode==selectedProperty)
    filteredSaleDataOutput<-datatable(filteredSaleData[,c('ROIpct','price','priceByArea',
                                                          'SuggestedRentalPrice','propertyType','size',
                                                          'rooms','floor','hasLift',
                                                          'exterior','status','address','url')],
                                      filter = 'none',
                                      colnames = c("Rentabilidad", "Precio", "Precio por m2",
                                                   "Alquiler estimado", "Tipo de propiedad", "m2",
                                                   "Habitaciones","Altura","Ascensor",
                                                   "Exterior","Estado", "Dirección", "Enlace"),
                                      options=list(pageLength=-1,
                                                   dom='tir'))
    output$filteredSaleData <-
      DT::renderDataTable(filteredSaleDataOutput)
  })
  
  #display Sale data
  saleDataOutput<-datatable(dadesSale[,c('ROIpct','price','priceByArea',
                                                        'SuggestedRentalPrice','propertyType','size',
                                                        'rooms','floor','hasLift',
                                                        'exterior','status','address','url')],
                                    filter = 'top',
                                    colnames = c("Rentabilidad", "Precio", "Precio por m2",
                                                 "Alquiler estimado", "Tipo de propiedad", "m2",
                                                 "Habitaciones","Altura","Ascensor",
                                                 "Exterior","Estado", "Dirección", "Enlace"),
                            options=list(pageLength=20,
                                         lengthMenu=c(10,20,50),
                                         dom='ltir'))
  output$saleData <-
    DT::renderDataTable(saleDataOutput)
  
  #display Rent data
  
  output$rentData <-DT::renderDataTable(datatable(
    dadesRent[,c('price','priceByArea','propertyType','size','rooms','floor','hasLift','exterior',
                 'status','address','url')],filter = 'top',
    colnames = c("Alquiler Mensual", "Precio por m2", "Tipo de propiedad", "m2","Habitaciones",
                 "Altura","Ascensor","Exterior","Estado", "Dirección", "Enlace"),
    options=list(pageLength=20,
                 lengthMenu=c(10,20,50),
                 dom='ltir')))
})
