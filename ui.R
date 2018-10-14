shinyUI(
  dashboardPage(skin = 'green',
        
      dashboardHeader(title = "Global Electricity (kWh-M)"),
    
      dashboardSidebar(
      
      sidebarUserPanel('by Phil Hopen'),
      
      sidebarMenu( id = 'sidebarmenu',
        menuItem("Energy Maps", tabName = "prod", icon = icon("map")),
        menuItem('Comparison', tabName = 'prop', icon = icon('map')),
        conditionalPanel("input.sidebarmenu == 'prod'",
                         selectizeInput(inputId = 'chlomaps',
                                        label = 'Choose activity: ',
                                        choices = c('Production', 'Consumption'))),
        
        conditionalPanel("input.sidebarmenu == 'prop'",
                         checkboxGroupInput('energy', 'Choose electricity source: ',
                                            c('Geothermal' = 'geothermal',
                                              'Hydro' = 'hydro',
                                              'Nuclear' = 'nuclear_electricity',
                                              'Solar' = 'solar_electricity',
                                              'Marine' = 'tide_wave_and_ocean_electricity',
                                              'Wind' = 'wind_electricity'))),
      
        conditionalPanel("input.sidebarmenu == 'prop'",
                         selectizeInput(inputId = 'country1',
                                        label = 'Country 1: ',
                                        choices = unique(combo$country_or_area))),
        conditionalPanel("input.sidebarmenu == 'prop'",
                         selectizeInput(inputId = 'country2',
                                        label = 'Country 2: ',
                                        choices = unique(combo$country_or_area))),
        
        
        
        sliderInput('yearRange', 'Range of Years:', min = 1994, 
                  max = 2014, value = c(1994,2014), sep = "")
      
    )),
    

    dashboardBody(
      tabItems(
        tabItem(tabName = "prod",
                fluidPage(
                  fillPage(plotlyOutput("both", height = '100%'))
                )),
        tabItem(tabName = 'prop',
                fluidPage(
                  fillPage(plotOutput('comp'))
                ))
      )
    )))