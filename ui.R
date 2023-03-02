library(shiny)
library(leaflet)

navbarPage("Rentabilidad de pisos", id="main",
        #    tags$style(HTML("
        # .navbar-default .navbar-brand {color: cyan;}
        # .navbar-default .navbar-brand:hover {color: blue;}
        # .navbar { background-color: gray;}
        # .navbar-default .navbar-nav > li > a {color:black;}
        # .navbar-default .navbar-nav > .active > a,
        # .navbar-default .navbar-nav > .active > a:focus,
        # .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
        # .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
        # .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
        # .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
        # .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}")),
           
           tabPanel("Mapa",
                    sidebarLayout(
                      #Panel lateral
                      sidebarPanel(width=3,
                                   
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
                                                                value = 100000, min=0, max=100000, step=10000))),
                                   
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
                                                                choices = c('Obra Nueva','Buen Estado','A Reformar'))))

                                   ),
                         
                      #Panel central
                      mainPanel(leafletOutput("bbmap", height='600px'),
                                DT::dataTableOutput("filteredSaleData")))),
        
           tabPanel("Viviendas de compra", DT::dataTableOutput("saleData")),
           tabPanel("Viviendas de alquiler", DT::dataTableOutput("rentData")),
           tabPanel("Información",includeMarkdown("readme_shiny.md")))

