shinyUI(
  dashboardPage(
        
      dashboardHeader(title = "Global Electricity (1994 - 2014)", titleWidth = 350),
    
      dashboardSidebar(
      
      sidebarMenu( id = 'sidebarmenu',
        menuItem("Maps", tabName = "prod", icon = icon("map")),
        menuItem('Comparison', tabName = 'prop', icon = icon('line-chart')),
        conditionalPanel("input.sidebarmenu == 'prod'",
                         selectizeInput(inputId = 'chlomaps',
                                        label = 'Choose activity: ',
                                        choices = c('Renewable Production', 'Total Consumption'))),
        
  
        conditionalPanel("input.sidebarmenu == 'prop'|input.chlomaps == 'Renewable Production'",
                         checkboxGroupInput('energy', 'Choose electricity source: ',
                                            c('Geothermal' = 'geothermal',
                                              'Hydro' = 'hydro',
                                              'Nuclear' = 'nuclear_electricity',
                                              'Solar' = 'solar_electricity',
                                              'Marine' = 'tide_wave_and_ocean_electricity',
                                              'Wind' = 'wind_electricity'),
                                            selected = c('geothermal', 'hydro', 'nuclear_electricity',
                                                         'solar_electricity', 'tide_wave_and_ocean_electricity',
                                                         'wind_electricity'))),
      
        conditionalPanel("input.sidebarmenu == 'prop'",
                         selectizeInput(inputId = 'country1',
                                        label = 'Country 1: ',
                                        choices = sort(unique(combo$country_or_area)),
                                        selected = 'United States')),
        conditionalPanel("input.sidebarmenu == 'prop'",
                         selectizeInput(inputId = 'country2',
                                        label = 'Country 2: ',
                                        choices = sort(unique(combo$country_or_area)),
                                        selected = 'Angola')),
        
        
        
        sliderInput('yearRange', 'Range of Years:', min = 1994, 
                  max = 2014, value = c(1994,2014), sep = "")
      
    )),
    

    dashboardBody( 
      
      shinyDashboardThemes(
      theme = "onenote"),
      
      tabItems(
        tabItem(tabName = "prod",
                
                fluidRow(
                  column(3, offset = 5, titlePanel("Global Electricity   (Millions of kWh)"))),
                fluidRow(
                  plotlyOutput("both", height = '700px'))
                ),
        tabItem(tabName = 'prop',
                fluidRow(
                 column(6, offset = 4, h3("Renewable Electricity/Consumption    (%)"))),
                fluidRow(
                  plotOutput('comp')),
                fluidRow(infoBoxOutput("maxBox"),
                         infoBoxOutput("avgBox"),
                         infoBoxOutput("minBox"))
      )
    ))))
