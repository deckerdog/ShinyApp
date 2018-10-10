library(shiny)
library(dplyr)
library(ggplot2)

energy_data <-  read.csv('./data/Renewable_Stats.csv')


fluidPage(
  titlePanel("Global Electricity (1994 - 2004)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectizeInput(inputId = "Country",
                     label = "Country",
                     choices = unique(energy_data$country_or_area))
    ),
    mainPanel(plotOutput("Country"))
  )
  
  
  
  
  # sidebarLayout(
  #   
  #   sidebarPanel(
  #     img(src='DSimage.jpg',
  #                width = '25%')),
  #   
  #   
  #   mainPanel(
  #     
  #     tags$iframe(
  #     src ='https://www.youtube.com/watch?v=yO1EjyhTMPc',
  #     width = '640', height = '360')
  #    )
  # )
)



