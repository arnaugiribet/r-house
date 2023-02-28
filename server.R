library(shiny)

library(dplyr)

library(leaflet)

library(data.table)

library(DT)

shinyServer(function(input, output) {
  
  # Import Data and clean it
  load('01-data_download/03dadesWithROI.RData')
  setDT(dades)
  dades[,rentabilidad := round(rnorm(nrow(dades),8,1),2)]
  dades[,alquiler:= round(rentabilidad*(price*1.15)/12/100)]
  dades[,rentabilidad_col:= cut(rentabilidad,c(-Inf,6,8,9,10,Inf), labels=c('Inferior a 6','6-8','8-9','9-10','Superior a 10'))]
  
  # new column for the popup label
  
  dades <- mutate(dades, cntnt=paste0('<strong>Rentabilidad: </strong>',rentabilidad,
                                      '<br><strong>Precio:</strong> ', price,
                                      '<br><strong>Precio por m2:</strong> ', priceByArea,
                                      '<br><strong>Alquiler estimado:</strong> ', alquiler,
                                      '<br><strong>Tipo de propiedad:</strong> ',propertyType,
                                      '<br><strong>m2:</strong> ',size,
                                      '<br><strong>Habitaciones:</strong> ',rooms,
                                      '<br><strong>Altura:</strong> ',floor,
                                      '<br><strong>Ascensor:</strong> ',hasLift,
                                      '<br><strong>Exterior:</strong> ',exterior,
                                      '<br><strong>Estado:</strong> ',status,
                                      '<br><strong>Dirección:</strong> ',address,
                                      '<br><strong>Enlace:</strong> ',url))

  # create a color paletter for category type in the data file
  
  pal <- colorFactor(pal = c('#FF625E','#FFBE65','#F5EEA5', '#D2FDBB', '#77DD76'), 
                     domain = c('Inferior a 6', "6-8", '8-9', '9-10','Superior a 10'))
   
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
      leaflet(dades) %>% 
      addCircles(lng = ~longitude, lat = ~latitude) %>% 
      addTiles() %>%
      addCircleMarkers(data = dades, lat =  ~latitude, lng =~longitude, 
                       radius = 3, popup = ~as.character(cntnt), 
                       color = ~pal(rentabilidad_col),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=dades$rentabilidad_col,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
        })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
      dades[,c('rentabilidad','price','priceByArea','alquiler','propertyType','size','rooms','floor','hasLift','exterior',
               'status','address','url')],filter = 'top',
      colnames = c("Rentabilidad", "Precio", "Precio por m2", "Alquiler estimado", "Tipo de propiedad", "m2","Habitaciones",
                   "Altura","Ascensor","Exterior","Estado", "Dirección", "Enlace")))

  
})
