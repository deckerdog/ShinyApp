shinyUI(
  dashboardPage(
    dashboardHeader(title = "Global Electricity(kWh - HM)"),
    
    dashboardSidebar(
      
      sidebarUserPanel('by Phil Hopen'),
      
      sidebarMenu(
        menuItem("Production", tabName = "prod", icon = icon("map")),
      
      sliderInput('yearRange', 'Range of Years:', min = 1994, 
                  max = 2014, value = c(1994,2014), sep = ""),
      
      selectizeInput(inputId = 'chlomaps',
                     label = 'chlomaps',
                     choices = c('Production', 'Consumption'))
      
    )),
    

    dashboardBody(
      tabItems(
        tabItem(tabName = "prod",
                fluidPage(
                  fillPage(plotlyOutput("both", height = '100%'))
                ))
      )
    )))