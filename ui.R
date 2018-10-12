

fluidPage(
  titlePanel("Global Electricity(kWh - HM)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput('yearRange', 'Range of Years:', min = 1994, 
                  max = 2014, value = c(1994,2014), sep = "")
    ),
    
    mainPanel(
      plotlyOutput("RenewYRange"))
  ))
  
  
  
  
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



