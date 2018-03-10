library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(ggvis)

shinyServer(function(input, output) {

##################################
#     BEGIN: Medal Plot Tab      #
##################################
  
  output$medalPlot <- renderPlot({
    
#-- store inputs for future use, passed from from ui.R
    x <- input$sport
    y1 <- input$year[1]
    y2 <- input$year[2]
    medals_selected <- input$medal_select
    gender_selected <- input$gender_select
    gen_dis <- if(x != "-ALL-"){
      input$gender_or_dis
    }    else{
      "Gender"
    }
      
      
#---- Error Handling for missing Olympic data in 1916, 1940 and 1944 ----#
    output$text <- renderText({
      if (y1 >= 1940 & y2 <= 1944) {
        print(paste0("You selected years ", y1, " and ", y2,".  Due to World War II, no Olympic games were held during this period."))
      }
      else if (y1 == 1916 & y2 == 1916){
        print(paste0("You selected only ", y1, ".  Due to World War I, no Olympic games were held during this period."))
      }
    }) #end error handling for empty years
    
#-------- handling plot for sports ---------#
##-------- IF to handle ALL or one  -------##
    df_m <- if(x != "-ALL-"){
      df %>% filter(Sport == x)
    } else{
      df
      }
    
    medals <- df_m %>% 
      filter(Year >= y1 & Year <= y2) %>%
      filter(Medal %in% medals_selected) %>% 
      filter(Gender %in% gender_selected) %>%
      select(Year, Country, Discipline, Event, Gender, Medal) %>% 
      group_by(Year, Country, Discipline, Event, Gender, Medal) %>%
      distinct() %>%
      tally()

    ggplot(data = medals, aes(x = as.factor(Year))) +
      geom_bar(aes_string(fill = gen_dis),position = position_stack(reverse = TRUE)) +
      ylab('Medal Count') + 
      xlab('Year') +
      theme_economist() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
      scale_fill_economist()
    
  }) #end output medal_plot

##################################
#     BEGIN: Data Table Tab      #
##################################
  
  output$Table <- DT::renderDT({
    x <- input$sport
    y1 <- input$year[1]
    y2 <- input$year[2]
    medals_selected <- input$medal_select
    gender_selected <- input$gender_select
    
#-- pre-plot filtering to keep code clean --#
    df_t <- if(x != "-ALL-"){
      df %>% filter(Sport == x)
    } else{
      df
      }
    
    tbl1 <- df_t %>% 
      filter(Year >= y1 & Year <= y2) %>%
      filter(Medal %in% medals_selected) %>%
      filter(Gender %in% gender_selected) %>%
      select(Location, Year, Country, Sport, Discipline, Event, Gender, Athlete, Medal)
    
#----- creating and formatting the data table ----#    
    datatable(tbl1,
              extensions = c('ColReorder', 'Buttons'), 
              options = list(dom = 'Bfrtip',   #for options, visit https://datatables.net/reference/option/dom
                             buttons = list('colvis','copy', 'print', 
                                            list(extend = 'collection',
                                                 buttons = c('csv', 'excel', 'pdf'),
                                                 text = 'Download')
                             ),
                             colReorder = list(realtime = FALSE)
              )
    ) %>%  formatStyle("Year", target = "row", backgroundColor = '#014d64',
                       class = 'compact')     
  })  #end output DataTable
  
##################################
#    BEGIN: Country Plot Tab     #
##################################
  
  
  output$countryPlot <- renderPlot({
    
#-- store inputs for future use, passed from from ui.R
    x <- input$sport
    y1 <- input$year[1]
    y2 <- input$year[2]
    medals_selected <- input$medal_select
    gender_selected <- input$gender_select
    
#-- pre-plot filtering to keep code clean --#
    df_c <- if(x != "-ALL-"){
      df %>% filter(Sport == x)
    } else{
      df
      }
    
    
    country_prep <- df_c %>% 
      filter(Year >= y1 & Year <= y2) %>%
      filter(Medal %in% medals_selected) %>% 
      filter(Gender %in% gender_selected) %>%
      select(Year, Country, Sport, Discipline, Event, Medal) %>%
    ###################################################################################
    #- calculating Medal Count by metal to display medals stacked for each country
    #-- since the data has athlete level detail, I use Group By and Distinct to 
    #--- count medals for a Sport -> Discipline -> Event -> Medal level to avoid 
    #---- counting each athlete within a team event. (e.g. hockey, 4x100m relay)
    ###################################################################################         
    group_by(Year, Country, Sport, Discipline, Event, Medal) %>%
      distinct() %>%
      tally() %>%
      ungroup() %>%
      group_by(Country) %>%
      mutate(Total_Medals = sum(n), sort = TRUE) %>%
      ungroup() %>%
      arrange(.,Total_Medals)
    
#---- begin filter for only Top # of Countries select ----#
    if(input$topX != "-ALL-"){
      country_topX <- country_prep %>% 
        select(Country, Total_Medals) %>% 
        distinct() %>% 
        arrange(desc(Total_Medals)) %>% 
        select(Country) %>% 
        head(n=as.numeric(input$topX))    
    }
    else{
      country_topX <- country_prep %>% 
        select(Country) %>% 
        distinct()  
    }
    
#---- plotting medals by metal, for each country ----#      
    country_plot <- country_prep %>% 
      filter(Country %in% unlist(country_topX))
    
    ggplot(country_plot, aes(x = reorder(Country, -Total_Medals), y = n, fill = factor(Medal, levels = c("Gold", "Silver", "Bronze")))) +
      geom_bar(stat = 'identity') +
      scale_fill_manual(name = "Medals", 
                        values = c("Gold" = "#F9C527", #orange F16C20
                                   "Silver" = "#CCCCCC", #blue 1395BA
                                   "Bronze" = "#C4542F")) + #dark blue 0D3C55
      ylab('Medal Count') +
      xlab('Country') +
#      theme_economist() +
      theme(panel.background = element_rect("#4e5d6c"), panel.grid.major.x = element_blank()) +  #grid background blue and rm vert gridlines 
      #theme(plot.border = element_rect("#1395BA")) + 
      theme(plot.background = element_rect("#2b3e50")) +

      theme(legend.background = element_rect("#2b3e50")) +
      theme(legend.text = element_text(color = "white")) +
      theme(legend.title = element_text(color = "white")) +
      
      theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2, color = "white")) +
      theme(axis.text.y = element_text(color = "white")) +
      theme(axis.title.x = element_text(color = "white")) +
      theme(axis.title.y = element_text(color = "white"))
      

        
    
  }) #-end country plot
  
##################################
#      BEGIN: Reference Tab      #
##################################
  
  output$ref <- renderUI({
    HTML(paste(h3("Sources used for this data project:"),
      tags$a(href = "https://www.theguardian.com/sport/datablog/2012/jun/25/olympic-medal-winner-list-data", "The Guardian, Olympic medal winners: every one since 1896 as open data"),
      br(),
      tags$a(href = "https://html-color-codes.info/colors-from-image/", "Select hex color from images"),
      br(),br(),
      h4("Special Thanks to ", a(href = "https://stackoverflow.com/questions/tagged/shiny", "Stackoverflow"), " and ", a(href = "https://www.DataCamp.com", "DataCamp")),
      hr(),
      h3("Other Information:"),
      tags$a(href = "https://www.github.com/DataScienceBS", "DataScienceBS Github Account"),
      br(),
      tags$a(href = "https://www.linkedin.com/in/BSanders21", "Brandon Sanders LinkedIn"),
      br(),
      tags$a(href = "http://nashvillesoftwareschool.com", "Nashville Software School")
      ))
    })
  
}) # end_server
