##################################################
#     creating a shiny app using olympic data    #
#   experience level: 40 days of exposure to R   #
#       project length: 2/15/18 - 3/10/18        #
##################################################
#                   data prep                    #
#    Testing plots and pipelines before shiny    #
##################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

##------------------ data prep ------------------##

winners <- read_excel("data/winners.xlsx")
country_cross <- read_excel("data/country_cross.xlsx")

df <- merge(x = winners, y = country_cross, by = "NOC", all.x = TRUE)

saveRDS(df, "olympics.RDS")


##------------------testing presentation material ------------------##
## Select Sport, display gold medal by country

medal_sport = df %>% 
  filter(Sport == "Aquatics") %>% 
  select(Country, Discipline, Medal, Year) %>% 
  group_by(Country, Discipline, Year, Medal) %>% 
  tally %>%
  mutate('Medal Count' = n) %>% 
  ggplot(., aes(x = Year, color = Discipline)) + 
  geom_bar() +
  ylab('Medal Count')
ggplotly(medal_sport)

#change point to bar and show medals ?
# fix axis labels and tooltips

####################################################################################
#    plotly not yet working.  bar orientation is horizontal instead of vertical    #
####################################################################################
df %>%
  filter(Sport == "Aquatics") %>%
  select(Country, Discipline, Medal, Year) %>%
  group_by(Country, Discipline, Year, Medal) %>%
  tally %>%
  mutate('Medal Count' = n) %>%
  plot_ly(.,x = ~Year, color = ~Discipline, type = "bar") %>% 
  layout(yaxis = list(title = 'Count'), barmode = 'group')
####################################################################################

df %>% 
  select(Year, Gender, Medal) %>% 
  group_by(Year, Gender) %>% 
  ggplot(., aes(x = Year, color = Gender)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  ylab('Medal Count')



####################################################################################
#     ---     testing table display of sport / country / year filtering    ---     #
####################################################################################


df %>% 
  select(Year, Gender, Medal) %>% 
  group_by(Year, Gender) %>% 
  ggplot(., aes(x = Year, fill = Gender)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  ylab('Medal Count') 



DTOutput(
  datatable(df)
)

datatable(df) %>% 
  filter(Sport == x) %>%
  filter(Year >= y1 & Year <= y2) %>%
  select(Location, Year, Country, Sport, Discipline, Event, Athlete, Gender, Medal)

