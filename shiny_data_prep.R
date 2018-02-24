##################################################
#     creating a shiny app using olympic data    #
#   experience level: 40 days of exposure to R   #
#       project length: 2/15/18 - 3/10/18        #
##################################################
#                   data prep                    #
#    Testing plots and pipelines before shiny    #
##################################################

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)
library(ggthemes)


##------------------ data prep ------------------##

winners <- read_excel("data/winners.xlsx")
country_cross <- read_excel("data/country_cross.xlsx")

df <- merge(x = winners, y = country_cross, by = "NOC", all.x = TRUE)

saveRDS(df, "Summer_Olympics/olympics.RDS")


##------------------testing presentation material ------------------##

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


##### looking at ISO country codes ######
library(countrycode)
library(maps)
library(mapdata)

ISO_convert <- read_excel('data/ISO_crosswalk.xlsx')
map_df <- merge(x = df, y = ISO_convert, by.x = "NOC", by.y = "Int Olympic Committee code", all.x = TRUE)

na_countries <- map_df %>% 
  filter(is.na(Country.y)) %>% 
  select(NOC, Country.x) %>% 
  distinct(Country.x)

temp <- data(worldMapEnv)


######### TBD #########
# Australasia = other
# Bohemia = CZE
# British West Indies = ???????????
# Unified team of Germany = DEU
# West Germany = DEU
# East Germany = DEU
# Independent Olympic Participants = other
# Romania = ROU
# Czechoslovakia = CZE
# Soviet Union = RUS
# Yugoslavia = YUG (source: https://www.iso.org/files/live/sites/isoorg/files/archive/pdf/en/iso_3166-3_newsletter_i-3.pdf)
# Mixed teams = other
#######################


#############################################
#  testing top 10 countries by medal count  #
#############################################

top10 <- df %>% 
  group_by(Country, Medal) %>% 
  count(Country, Medal) %>% 
  summarise("Medals" = sum(n)) %>%  
  summarise("Total Medals" = sum("Medals"))
  arrange(Country, Medal) %>% 
  head(n=10)

top_plot <- df %>% 
  filter(Country %in% top10$Country) %>% 
  group_by(Country, Medal) %>% 
  count("Medal Count" = n(), sort = TRUE) %>% 
  select(Country, Medal, "Medal Count") %>% 
  ggplot(., aes(x = "Medal Count", y = Country)) +
  geom_bar(fill = Medal)


library(ggthemes)

test <- df %>% 
  filter(Sport == "Gymnastics") %>%
  filter(Year >= 2008 & Year <= 2008) %>%
  filter(Medal == "Bronze") %>% 
  filter(Gender == "Women") %>%
  select(Year, Country, Sport, Discipline, Event, Medal) %>%
#  aggregate(.,by = list(Year, Country, Sport, Discipline, Event, Medal))
  group_by(Year, Country, Sport, Discipline, Event, Medal) %>%
  distinct() %>% 
#  count(Year, Country, Sport, Discipline, Event, Medal, sort = TRUE) %>%
  mutate(Medal_Count = n()) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(Total_Medals = sum(n)) %>%
  ungroup() 
  

View(test)
