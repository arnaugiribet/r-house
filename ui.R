library(shiny)
library(leaflet)

navbarPage("Rentabilidad de pisos", id="main",
           tabPanel("Mapa", leafletOutput("bbmap", height='600px')),
           tabPanel("Datos", DT::dataTableOutput("data")),
           tabPanel("Información",includeMarkdown("readme_shiny.md")))

