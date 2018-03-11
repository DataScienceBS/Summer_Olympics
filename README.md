# Exploring Summer Olympic Medal Winners  
 
### Author Background  
This project was initiated in February, 2018, while enrolled in the [Nashville Software School, Data Science Bootcamp, Cohort 1](http://www.nashvillesoftwareschool.com). The Data Science cohort started October 17th, 2018, and began with 10 weeks of in depth Python learning. This included 3 Data Question projects using open data. Upon creation of this project, the Data Science Cohort had spent 6 weeks learning R to prepare for our first Shiny App. Upon starting classes at NSS, I had limited coding experience beyond my undergraduate studies over 10 years prior. This shiny app represents the half way mark of the cohort, applying 10 weeks of Python and 6 weeks of R exposure.

### Schedule of Work  
1.	Obtain  Data (February 6th)
2.	Clean & Explore the Data (February 15th)
3.	Build & Deploy Shiny App (February 27th)
4.	Document/Pitch Shiny App with a Presentation (March 10th)
  
### The Project  
The data contains all medalists from the Summer Olympic games through 2008, stretching back to the establishment of the Games of the Olympiad in 1896. It is structured by Olympic Year and Location, the Sport (Aquatics), Discipline (Swimming), Event (1500m relay), Medal (Gold), Medalists (including each team athlete), and Gender along with the Country represented.

### Data Work
The granularity of the data includes athlete names, so with team sports (relays, hockey, basketball), it shows every athlete who earned a medal. To ensure plots reflect actual totals for each country or sport, the data needs to be summarized in a few different ways. First, for the Medals by Sport, we need medals won by year, country, sport, discipline, event, gender, and medal. Because a country can earn more than one medal in the 4x100m relay, and also win Gold in the same event for each Men and Women. This must be considered when aggregating the data to arrive at accurate numbers.

The aggregation of data for plotting Medals by Country is two-fold. Because of the number of countries in the data, the bar chart should be ordered in descending from left to right by most medals earned. Additionally, the bar chart will be filled by the number of medals earned by metal - Gold, Silver, and Bronze. Using ggplot, hex colors will be assigned to the medal metals to easily identify the medals won be each country.

### Formatting  
**Page Display**  
Within the shiny app, a side panel and main panel were arranged to allow for the best presentation of filter options and plot display. Within the side panel, a user if able to select whether to view all sports, or one specific sport, to display in the main panel. If viewing all sports, the Medals by Sport plot will fill color in the bar chart by Gender, showing the medals won by year for both Men and Women. A conditional panel was included in the side panel to give added visibility - if a specific sport is selected, the user is now able to choose whether to view the chart color filled by Gender, or by the disciplines under that sport. This provides more insight into the increase in medals awarded over time. 

Using TabPanels, the data was displayed in the main plot with tabs for Medals by Sport, a tab to display the data as filtered, a tab to display a plot for Medals by Country, and a Reference tab.
   
**Color Formatting**  
I applied the superhero shiny theme to add style. The hex codes for the shiny theme were also applied manually to the plot and panel in the Medals by Country tab. The shiny theme, unfortunatley, did not apply well to the DataTable package. I chose to work with a teal background with light blue hover for readability of the data. The navigation buttons for the table, as well as the page numbers, were difficult to read because of dark text on a dark background. Both the hover color and the buttons were remedied in ui.R by applying HTML style tags to compliment the theme palette and improve visibility. 

**User Interaction**  
While investigating options for styling the dataTables package, I decided to provide ability for the user to interact with the table. The user is able to drag the columns into their preferred order, and with the Column Visibility button, choose which columns to show or hide. The user can choose to pull the data for themselves with the buttons added to copy, print, or download (CSV, PDF, or Excel). 

The Medals by Country plot displays all countries who have earned a medal within the sport, reflective of the user filters applied in the side panel. Because the data can be overwhelming when displaying all countries, options were provided to the user to show All data, or show only the Top 5, 10, 25, or 50 countries.  
  
  
### Data Sources  
[Summer Olympic Data from The Guardian, 1896 - 2008](https://www.theguardian.com/news/datablog/2010/feb/11/winter-olympics-medals-by-country)  

