
shinyUI(
dashboardPage(
  dashboardHeader(title = "Global Electricity(kWh - HM)"),
  
  dashboardSidebar(
    
    sidebarUserPanel('by Phil Hopen'),
    
    sidebarMenu(
      menuItem("Production", tabName = "prod", icon = icon("map")),
      menuItem("Consumption", tabName = "con", icon = icon("database"))),
      
      sliderInput('yearRange', 'Range of Years:', min = 1994, 
                  max = 2014, value = c(1994,2014), sep = "")
    ),
    
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "prod",
    fluidPage(
      fillPage(plotlyOutput("RenewYRange", height = '100%'))
  )),
  tabItem(tabName = "con",
          fluidPage(
            fillPage(plotlyOutput("ConsYRange", height = '100%'))
          ))
  )
)))
  
  
  
  
#   # sidebarLayout(
#   #   
#   #   sidebarPanel(
#   #     img(src='DSimage.jpg',
#   #                width = '25%')),
#   #   
#   #   
#   #   mainPanel(
#   #     
#   #     tags$iframe(
#   #     src ='https://www.youtube.com/watch?v=yO1EjyhTMPc',
#   #     width = '640', height = '360')
#   #    )
#   # )
# )



