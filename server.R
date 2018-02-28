library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

shinyServer(function(input, output) {
    
  output$medalPlot <- renderPlot({
    
    # store inputs for future use, passed from from ui.R
    x <- input$sport
    y1 <- input$year[1]
    y2 <- input$year[2]
    medals_selected <- input$medal_select
    gender_selected <- input$gender_select
    
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
    ##-------- IF to handle ALL or One  -------##
    df <- if(x != "-ALL-"){
      df %>% filter(Sport == x)
    } else{df}
    
    df %>% 
      filter(Year >= y1 & Year <= y2) %>%
      filter(Medal %in% medals_selected) %>% 
      filter(Gender %in% gender_selected) %>%
      select(Year, Country, Discipline, Event, Gender, Medal) %>% 
      group_by(Year, Country, Discipline, Event, Gender, Medal) %>%
      distinct() %>%
      tally() %>% 
      ggplot(., aes(x = as.factor(Year))) +
      geom_bar(aes_string(fill = input$gender_or_dis),position = position_stack(reverse = TRUE)) +
      ylab('Medal Count') + 
      xlab('Year') +
      theme_economist() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
      scale_fill_economist()
  }) #end output medal_plot
  
  
  #------- handling table data as second tab -------#
  
  output$Table <- DT::renderDT({
    x <- input$sport
    y1 <- input$year[1]
    y2 <- input$year[2]
    medals_selected <- input$medal_select
    gender_selected <- input$gender_select
    
    #-- pre-plot filtering to keep code clean --#
    df <- if(x != "-ALL-"){
      df %>% filter(Sport == x)
    } else{df}
    
    tbl1 <- df %>% 
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
  
  
  output$countryPlot <- renderPlot({
    
    # store inputs for future use, passed from from ui.R
    x <- input$sport
    y1 <- input$year[1]
    y2 <- input$year[2]
    medals_selected <- input$medal_select
    gender_selected <- input$gender_select
    
    #-- pre-plot filtering to keep code clean --#
    df <- if(x != "-ALL-"){
      df %>% filter(Sport == x)
    } else{df}
    
    
    country_prep <- df %>% 
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
                        values = c("Gold" = "#C98910",
                                   "Silver" = "#A8A8A8",
                                   "Bronze" = "#965A38")) +
      ylab('Medal Count') +
      xlab('Country') +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2))
    
  }) #-end country plot
  
  
}) # end_server
