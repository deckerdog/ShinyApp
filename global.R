library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
#install.packages('devtools')
#install_github("nik01010/dashboardthemes")
#library(devtools)
library(dashboardthemes)


energy_data <-  read.csv('./Final_Renewable_Stats.csv')
consumption_data <- read.csv('./Final_Consumption_Stats.csv')
combo <- read.csv('./Final_Combined_Stats.csv')
