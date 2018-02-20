library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

df <- readRDS("olympics.RDS")

min_date <- min(df$Year)
max_date <- max(df$Year)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
 # Application title
   titlePanel("Summer Olympic Medal Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # selectInput(inputId = "country", 
        #              label = "Country: ", 
        #              choices = sort(c(df$Country,"-ALL-")),
        #              selected = "-ALL-", 
        #              multiple = FALSE,
        #              width = NULL, 
        #              size = NULL),  
  
        selectInput(inputId = "sport", 
                    label = "Sport: ", 
                    choices = sort(c(df$Sport,"-ALL-")),
                    selected = "-ALL-", 
                    multiple = FALSE,
                    width = NULL, 
                    size = NULL),  

        checkboxGroupInput("medal_select", 
                           label = "Medals to Display", 
                           choices = c("Gold", "Silver", "Bronze"), 
                           selected = c("Gold", "Silver", "Bronze")),
        
        checkboxGroupInput("gender_select", 
                           label = "Gender to Display", 
                           choices = c("Men", "Women"), 
                           selected = c("Men", "Women")),

        sliderInput("year",
                    "Drill down to a specific range of years for a better visual: ",
                    min = min_date,
                    max = max_date,
                    sep = "",
                    step = 4,
                    value = c(min_date, max_date)),

#----------- only show this panel if not reviewing ALL sports (input$sport) -----------#
### because of data complexity, we will not show all disciplines across all sports ###

        conditionalPanel(
          condition = "input.sport != '-ALL-'",
          radioButtons(inputId = "gender_or_dis", 
                    label = "Stack by Sport Discipline or by Gender: ", 
                    choices = c("Discipline","Gender"),
                    selected = "Gender"))
        ),
      
      
      # Show plots of the selected inputs
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", textOutput("text"), plotOutput("medalPlot")),
          tabPanel("Table",   DT::dataTableOutput("Table"))
        )
      )
   )
)
###########################################
############# start of server #############
###########################################

server <- function(input, output) {

  output$medalPlot <- renderPlot({

      # store inputs for future use, passed from from ui.R
      x <- input$sport
      y1 <- input$year[1]
      y2 <- input$year[2]
      medals_selected <- input$medal_select
      gender_selected <- input$gender_select
      
#---- Error Handling for missing Olympic data 1940 and 1944 ----#
      output$text <- renderText({
          if (y1 >= 1940 & y2 <= 1944) {
            print(paste0("You selected years ", y1, " and ", y2,".  Due to World War II, no Olympic games were held during this period."))}
          else if (y1 == 1916 & y2 == 1916){
            print(paste0("You selected only ", y1, ".  Due to World War I, no Olympic games were held during this period."))}
  
        }) #end error handling for empty years

#-------- handling plot for sports ---------#
##-------- IF to handle ALL or One  -------##

      if (x == "-ALL-") {
        df %>% 
          filter(Year >= y1 & Year <= y2) %>%
          filter(Medal %in% medals_selected) %>% 
          filter(Gender %in% gender_selected) %>%
          select(Year, Gender, Medal) %>% 
          group_by(Year, Gender) %>% 
          ggplot(., aes(x = Year, fill = Gender)) +
          geom_bar(position = position_stack(reverse = TRUE)) +
          ylab('Medal Count') 
          } 
      else{
        df %>%
          filter(Sport == x) %>%
          filter(Year >= y1 & Year <= y2) %>%
          filter(Medal %in% medals_selected) %>% 
          filter(Gender %in% gender_selected) %>%
          select(Country, Discipline, Gender, Medal, Year) %>%
            group_by(Country, Discipline, Gender, Year, Medal) %>%
            tally %>%
            mutate('Medal Count' = n) %>%
            ggplot(., aes_string(x = "Year", fill = input$gender_or_dis)) +
            geom_bar(position = position_stack(reverse = TRUE)) +
            ylab('Medal Count') 
          }
  }) #end output medal_plot
  
############## plotly option ----- still in development -----
        # df %>%
        #   filter(Sport == x) %>%
        #   select(Country, Discipline, Medal, Year) %>%
        #   group_by(Country, Discipline, Year, Medal) %>%
        #   tally %>%
        #   mutate('Medal Count' = n) %>%
        #   plot_ly(x = ~Year, color = ~Discipline, type = "bar")
###############

  
#------- handling table data as second tab -------#
   
   output$Table <- DT::renderDataTable({
     x <- input$sport
     y1 <- input$year[1]
     y2 <- input$year[2]
     medals_selected <- input$medal_select
     gender_selected <- input$gender_select
     
     if (x == "-ALL-") {
       df %>% 
         filter(Year >= y1 & Year <= y2) %>%
         filter(Medal %in% medals_selected) %>%
         filter(Gender %in% gender_selected) %>%
         select(Location, Year, Country, Sport, Discipline, Event, Gender, Athlete, Medal) 
        } 
     else{
       df %>%
         filter(Sport == x) %>%
         filter(Year >= y1 & Year <= y2) %>%
         filter(Medal %in% medals_selected) %>% 
         filter(Gender %in% gender_selected) %>%
         select(Location, Year, Country, Sport, Discipline, Event, Gender, Athlete, Medal)
        }  #end if/else  

  })  #end output DataTable
 
} # end_server

# Run the application 
shinyApp(ui = ui, server = server)
