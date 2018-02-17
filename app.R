library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

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
        selectInput(inputId = "sport", 
                     label = "Select the Sport to see the medal count by year for each discipline ", 
                     choices = sort(c(df$Sport,"ALL")),
                     selected = "ALL", 
                     multiple = FALSE,
                     width = NULL, 
                     size = NULL),  
  
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
          condition = "input.sport != 'ALL'",
          radioButtons(inputId = "gender_or_dis", 
                    label = "Stack by Sport Discipline or by Gender: ", 
                    choices = c("Discipline","Gender"),
                    selected = "Gender"))

        ),
      
      
      # Show a plot of the selected inputs
      mainPanel(
        textOutput("text"),
        plotOutput("medalPlot")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$medalPlot <- renderPlot({
      # generate bins based on input$sport and input$year from ui.R
      x <- input$sport
      y1 <- input$year[1]
      y2 <- input$year[2]

      output$text <- renderText({
          if (y1 >= 1940 & y2 <= 1944) {
            print(paste0("You selected years ", y1, " and ", y2,".  Due to World War II, no Olympic games were held during this period."))}
        })

      
        
#################################      
#  code is working - backup 1   #
#################################     
      # bar chart shown with medal counts by discipline of the selected sport, by year
      # df %>%
      #   filter(Sport == x) %>%
      #   filter(Year >= y1 & Year <= y2) %>% 
      #   select(Country, Discipline, Medal, Year) %>%
      #   group_by(Country, Discipline, Year, Medal) %>%
      #   tally %>%
      #   mutate('Medal Count' = n) %>%
      #   ggplot(., aes(x = Year, color = Discipline)) +
      #   geom_bar() +
      #   ylab('Medal Count')
###################      

    
##################################      
#  code below is working - KEEP  #  
##################################     
      if (x == "ALL") {
        df %>% 
          filter(Year >= y1 & Year <= y2) %>%
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
          select(Country, Discipline, Gender, Medal, Year) %>%
          group_by(Country, Discipline, Gender, Year, Medal) %>%
          tally %>%
          mutate('Medal Count' = n) %>%
          ggplot(., aes_string(x = "Year", fill = input$gender_or_dis)) +
          geom_bar(position = position_stack(reverse = TRUE)) +
          ylab('Medal Count') 
        
        }
      
      
############## plotly option ----- still in development -----
        # df %>%
        #   filter(Sport == x) %>%
        #   select(Country, Discipline, Medal, Year) %>%
        #   group_by(Country, Discipline, Year, Medal) %>%
        #   tally %>%
        #   mutate('Medal Count' = n) %>%
        #   plot_ly(x = ~Year, color = ~Discipline, type = "bar")
###############
      
      
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

