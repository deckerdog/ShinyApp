library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)


energy_data <-  read.csv('./Final_Renewable_Stats.csv')
consumption_data <- read.csv('./Final_Consumption_Stats.csv')
