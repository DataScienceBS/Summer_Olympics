#############################
#   after 1 week of dev    
#    I decided I needed
#  more flexibility. This
#  app is testing fluidRows
#############################

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)
library(ggthemes)


df <- readRDS("olympics.RDS")

min_date <- min(df$Year)
max_date <- max(df$Year)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),
#ui <- fluidPage(theme = shinytheme("flatly"),
#ui <- fluidPage(theme = shinytheme("superhero"),
#ui <- fluidPage(theme = shinytheme("yeti"),

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
          tabPanel("Annual Plot", textOutput("text"), plotOutput("medalPlot")),
          tabPanel("Table",   DT::dataTableOutput("Table")),
          tabPanel("Country Plot", plotOutput("countryPlot"))
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
      radio1 <- input$gender_or_dis
      
      
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
          ggplot(., aes(x = as.factor(Year), fill = Gender)) +
          geom_bar(position = position_stack(reverse = TRUE)) +
          ylab('Medal Count') + 
          xlab('Year') +
          theme_economist() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
          scale_fill_economist()
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
            ggplot(., aes(x = as.factor(Year))) +
            geom_bar(aes_string(fill = input$gender_or_dis),position = position_stack(reverse = TRUE)) +
            ylab('Medal Count') +
            xlab('Year') +
            theme_economist() + 
            theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
            scale_fill_economist()
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
       tbl1 <- df %>% 
         filter(Year >= y1 & Year <= y2) %>%
         filter(Medal %in% medals_selected) %>%
         filter(Gender %in% gender_selected) %>%
         select(Location, Year, Country, Sport, Discipline, Event, Gender, Athlete, Medal)
        } 
     else{
       tbl1 <- df %>%
         filter(Sport == x) %>%
         filter(Year >= y1 & Year <= y2) %>%
         filter(Medal %in% medals_selected) %>% 
         filter(Gender %in% gender_selected) %>%
         select(Location, Year, Country, Sport, Discipline, Athlete, Gender, Medal, Event)
        }  #end if/else  

     datatable(tbl1) %>% formatStyle("Year", target = "row",
       backgroundColor = "#3E647D"
       ,class = 'compact'
     )     
  })  #end output DataTable


   
   
output$countryPlot <- renderPlot({
     
     # store inputs for future use, passed from from ui.R
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
         select(Country, Year, Medal) %>%
         group_by(Country, Year, Medal) %>%
         tally() %>%
         mutate(Medal_Count = n) %>%
         ungroup() %>% 
         group_by(Country) %>% 
         mutate(Total_Medals = sum(n), sort = TRUE) %>%
         ungroup() %>% 
         ggplot(., aes(x = reorder(Country, -Total_Medals), y = Medal_Count, fill = factor(Medal, levels = c("Gold", "Silver", "Bronze")))) +
         geom_bar(stat = 'identity') +
         scale_fill_manual("Medals", values = c("#C98910", "#A8A8A8", "#965A38")) +
         ylab('Medal Count') +
         xlab('Country') +
         theme_economist() + 
         theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) 
        }
     else{     
       df %>% 
         filter(Sport == x) %>%
         filter(Year >= y1 & Year <= y2) %>%
         filter(Medal %in% medals_selected) %>% 
         filter(Gender %in% gender_selected) %>%
         select(Country, Year, Medal) %>%
         group_by(Country, Year, Medal) %>%
         tally() %>%
         mutate(Medal_Count = n) %>%
         ungroup() %>% 
         group_by(Country) %>% 
         mutate(Total_Medals = sum(n), sort = TRUE) %>%
         ungroup() %>% 
         ggplot(., aes(x = reorder(Country, -Total_Medals), y = Medal_Count, fill = factor(Medal, levels = c("Gold", "Silver", "Bronze")))) +
         geom_bar(stat = 'identity') +
         scale_fill_manual("Medals", values = c("#C98910", "#A8A8A8", "#965A38")) +
         ylab('Medal Count') +
         xlab('Country') +
         theme_economist() + 
         theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) 
         }
     
    }) #-end country plot

} # end_server

# Run the application 
shinyApp(ui = ui, server = server)




