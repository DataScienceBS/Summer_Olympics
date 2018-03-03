library(shiny)
library(DT)
library(shinythemes)
library(ggthemes)
library(ggvis)

shinyUI(fluidPage(theme = shinytheme("superhero"),

          # Application title
                titlePanel(
                  fluidRow(
                    column(1,HTML("<img src='transparent_rings.png', width = 100, style='background-color:transparent, border:0px, box-shadow:none'>")),
                    column(10,
                           HTML("<p align=center>Exploring Summer Olympic Data, 1896 - 2008</p><br/>")),
                    column(1)
                          ),
                  windowTitle = "Summer Olympics"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "sport", 
                                label = "Sport: ", 
                                choices = sort(c(df$Sport,"-ALL-")),
                                selected = "-ALL-", 
                                multiple = FALSE,
                                width = NULL, 
                                size = NULL),  
                    
                    fluidRow(
                      column(3,
                             checkboxGroupInput("medal_select", 
                                                label = "Medals: ", 
                                                choices = c("Gold", "Silver", "Bronze"), 
                                                selected = c("Gold", "Silver", "Bronze"))
                      ),
                      column(4,
                             checkboxGroupInput("gender_select", 
                                                label = "Gender: ", 
                                                choices = c("Men", "Women"), 
                                                selected = c("Men", "Women"))
                      ),
                      
                      #---------- only show this panel if not reviewing ALL sports (input$sport) ----------#
                      ### for visualization purposes, we will not show all disciplines across all sports ###
                      column(4,
                               conditionalPanel(
                               condition = "input.sport != '-ALL-'",
                               radioButtons(inputId = "gender_or_dis", 
                                            label = "Group By: ", 
                                            choices = c("Discipline","Gender"),
                                            selected = "Gender"))
                      )), #--end fluidRow
                    
                    sliderInput("year",
                                "Drill down to a specific range of years for a better visual: ",
                                min = min_date,
                                max = max_date,
                                sep = "",
                                step = 4,
                                value = c(min_date, max_date))
                  , width = 3),  #--end sidebar panel
                  
                  
                  # Show plots of the selected inputs
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Medals by Sport", textOutput("text"), plotOutput("medalPlot")),
                      tabPanel("Table",
                               tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #01a2d9 !important;
                                                         }
                                                         "))),
                               tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                                               color: #6794a7 !important;
                                               }")),
                               DT::dataTableOutput("Table")),
                      tabPanel("Medals by Country", 
                               fluidRow(
                                 column(4),
                                 column(4,selectInput("topX",
                                                      label = "Show Top: ",
                                                      choices = c("-ALL-","5","10","25","50"),
                                                      selected = '-ALL-',
                                                      multiple = FALSE,
                                                      width = NULL,
                                                      size = NULL)),
                                 column(4)
                               ), #--- end fluid header row ---#
                               plotOutput("countryPlot")),
                      tabPanel("Reference", htmlOutput("ref"))                    )
                  ,width = 9)
                )
))