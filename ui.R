library(shiny)
library(leaflet)

navbarPage("Rentabilidad de pisos", id="main",
           tabPanel("Mapa",
                    leafletOutput("bbmap", height='600px'),
                    DT::dataTableOutput("filteredSaleData")),
           tabPanel("Viviendas de compra", DT::dataTableOutput("rentData")),
           tabPanel("Viviendas de alquiler", DT::dataTableOutput("saleData")),
           tabPanel("Información",includeMarkdown("readme_shiny.md")))

