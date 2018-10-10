library(shiny)
library(dplyr)
library(ggplot2)

energy_data <-  read.csv('./data/Renewable_Stats.csv')
 

function(input, output) {
  
  output$Country <- renderPlot(
    energy_data %>%
      filter(country_or_area == input$Country) %>%
      group_by(year) %>%
      summarise(., Total_Hydro = sum(hydro)) %>%
      ggplot(aes(x = year, y = Total_Hydro)) +
      geom_col(fill = "lightblue") +
      ggtitle("Hydro kWh-hm")
  )
}