library(shiny)
library(leaflet)

navbarPage("Rentabilidad de pisos", id="main",
           tabPanel("Mapa",
                    fluidRow(sidebarLayout(
                      #Panel lateral
                      sidebarPanel(style='height: 600px',width=3,
                                   
                                   fluidRow(column(12, # Rentabilidad
                                                   checkboxGroupInput("roiSelectInput",
                                                                      "Rentabilidad",
                                                                      choices = c('Inferior al 6%','6-8%','8-9%','9-10%','10-12%','Superior al 12%')))),
                                   
                                   fluidRow(column(6, # Preu mín
                                                   numericInput("priceMinSelectInput",
                                                                "Precio Mín.",
                                                                value = 0, min=0, max=100000, step=10000)),
                                            column(6, #Preu max
                                                   numericInput("priceMaxSelectInput",
                                                                "Precio Máx.",
                                                                value = NA, min=0, max=100000, step=10000))),
                                   
                                   fluidRow(column(6, # Preu mín
                                                   numericInput("rentMinSelectInput",
                                                                "Aquiler Mín.",
                                                                value = 0, min=0, max=10000, step=100)),
                                            column(6, #Preu max
                                                   numericInput("rentMaxSelectInput",
                                                                "Aquiler Máx.",
                                                                value = NA, min=0, max=10000, step=100))),
                                   
                                   fluidRow(column(6, #Size min
                                                   numericInput("sizeMinSelectInput",
                                                                "Tamaño Mín.",
                                                                value = 0, min=0, max=9999, step=10)),
                                            column(6, #Size max
                                                   numericInput("sizeMaxSelectInput",
                                                                "Tamaño Máx.",
                                                                value = NA, min=0, max=9999, step=10))),
                                   fluidRow(column(12, # Estado
                                                   checkboxGroupInput("statusSelectInput",
                                                                      "Estado",
                                                                      choiceNames = c('Obra Nueva','Buen Estado','A Reformar'),
                                                                      choiceValues = c('newdevelopment','good','renew'))))
                                   
                      ),
                      
                      #Panel central
                      mainPanel(leafletOutput("saleMap", height='600px'))
                      )),
                    fluidRow(p(''),
                             splitLayout(cellWidths = c("2%","50%","2%","31%","10%"),
                                           '',
                                           div(DT::dataTableOutput("filteredRentData"), style='font-size:80%'),
                                           '',
                                           leafletOutput("rentMap", height='400px'),
                                           ''
                                           ),
                             p('')
                               )
                    ),
           tabPanel("Viviendas de compra", DT::dataTableOutput("saleData")),
           tabPanel("Viviendas de alquiler", DT::dataTableOutput("rentData")),
           tabPanel("Información",includeMarkdown("readme_shiny.md")))

