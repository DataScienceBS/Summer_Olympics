library(shiny)
library(DT)
library(shinythemes)
library(ggthemes)

#shinyUI(fluidPage(theme = shinytheme("slate"),
#shinyUI(fluidPage(theme = shinytheme("flatly"),
shinyUI(fluidPage(theme = shinytheme("superhero"),
#shinyUI(fluidPage(theme = shinytheme("yeti"),
                
                # Application title
                titlePanel("Summer Olympic Medal Data"),
                
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
                      column(5,
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
                  ),  #--end sidebar panel
                  
                  
                  # Show plots of the selected inputs
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Medals by Sport", textOutput("text"), plotOutput("medalPlot")),
                      tabPanel("Table",   DT::dataTableOutput("Table")),
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
                               plotOutput("countryPlot"))
                    )
                  )
                )
))