


function(input, output, session) {
  
#maps---------------------------------------------------------------------------------------------------------------
choice <- reactive({input$chlomaps})
output$both <-
  renderPlotly({
  if (choice() == 'Renewable Production') {
    
    
#Production data--------------------------------------------------------------------------------------------------
    
    energy_data_sums <- filter(energy_data, year >= input$yearRange[1], year <= input$yearRange[2]) %>% 
      group_by(., alpha.3) %>% 
      summarise(., geothermal = sum(geothermal), hydro = sum(hydro), nuclear_electricity = sum(nuclear_electricity), 
                solar_electricity = sum(solar_electricity), tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
                wind_electricity = sum(wind_electricity), total = sum(total), country_or_area = first(country_or_area)) %>% 
      ungroup(.) %>% 
      mutate(., select_total = round(log10(rowSums(select_(., .dots = input$energy))+1), 2))
    
      gs = c('geothermal', 'hydro', 'nuclear_electricity',
         'solar_electricity', 'tide_wave_and_ocean_electricity',
         'wind_electricity')
      gl = c('Geothermal', 'Hydro','Nuclear', 'Solar', 'Marine','Wind')
    
      nums <- select_(energy_data_sums,.dots = input$energy) %>% 
      mutate(., pc = rowSums(select_(., .dots = input$energy)))
  
     jkl = data.frame(gs,gl)
     jkl <- filter(jkl, gs %in% names(nums))
     names(nums) = jkl$gl
  
     ls <- data.frame()
     for (i in 1:nrow(nums)) { 
       for (j in 1:(ncol(nums)-1)) {
         ls[i,j] <-paste0(names(nums)[j],'-', round(nums[i,j]/(nums[i, ncol(nums)]+1)*100, 2),'%', '\n')
    }
  }
     
  
  
           
  lf = do.call("paste",ls)
  
  for (i in 1:length(lf)) {
    lf[i] <- paste0(energy_data_sums$country_or_area[i], '\n','\n', lf[i])
  }
  
  energy_data_sums$hover <- lf
    
    
  
  
    
#Production chloropleth --------------------------------------------------------------------------------------- 
    
  l <- list(color = toRGB("white"), width = 2)
    
  g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
  plot_geo(energy_data_sums) %>%
      add_trace(
        z = ~select_total, text = ~hover, locations = ~alpha.3,
        color = ~select_total, colors = 'Greens'
      ) %>%
      colorbar(title = "Log10(kWh-M)") %>%
      layout(
        title = '\n Production \n',
        geo = g
      )
  
  
  
  
  
#Consumption chloropleth-----------------------------------------------------------------------------------------
  
  } else {
    cons_data_sums <- group_by(consumption_data, alpha.3) %>% 
      filter(.,year >= input$yearRange[1], year <= input$yearRange[2]) %>% 
      summarise(., total = round(log10(sum(Total_Consumption)+1), 2), country_or_area = first(country_or_area))
    
    
    cons_data_sums$hover <- with(cons_data_sums, paste(country_or_area))
    
    
    l <- list(color = toRGB("white"), width = 2)
    
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(cons_data_sums) %>%
      add_trace(
        z = ~total, text = ~hover, locations = ~alpha.3,
        color = ~total, colors = 'Reds'
      ) %>%
      colorbar(title = 'Log10(kWh-M)') %>%
      layout(
        title = ' \n Consumption',
        geo = g
      )}
  })
  



  
#Reactive choices for Comparison tab----------------------------------------------------------------------------------
observe({
    
    country2 <- unique(combo %>%
                     filter(country_or_area != input$country1) %>%
                     .$country_or_area)
    
    updateSelectizeInput(
      session,'country2',
      choices = sort(country2),
      selected = country2[21])
  })
  
combo_filt <- reactive({
    filter(combo, year >= input$yearRange[1], year <= input$yearRange[2]) %>% 
    filter(.,country_or_area == input$country1 | country_or_area == input$country2 ) %>% 
    select_(.,.dots = c('year', 'alpha.3', 'country_or_area', 'unit', input$energy, 'Total_Consumption')) %>% 
    mutate(., prop = rowSums(select_(., .dots = input$energy))/Total_Consumption*100)
    })
   


 
  
#Comparison page--------------------------------------------------------------------------------------------------
output$comp <- renderPlot(ggplot(combo_filt(), aes(x = as.character(year), y = prop, group = country_or_area,
                                                     colour = country_or_area))+
                              geom_line() + geom_point() + theme(plot.subtitle = element_text(vjust = 1), 
                                                                 plot.caption = element_text(vjust = 1), 
                                                                 axis.line = element_line(colour = "azure4", 
                                                                                          linetype = "solid"), panel.grid.major = element_line(colour = "darkseagreen3"), 
                                                                 panel.grid.minor = element_line(colour = "darkseagreen3"), 
                                                                 plot.title = element_text(size = 18, 
                                                                                           hjust = 0.5), panel.background = element_rect(fill = "azure2"), 
                                                                 legend.key = element_rect(fill = "white"), 
                                                                 legend.background = element_rect(fill = "white")) +labs(x = "Year", y = "%", colour = "Country"))






#top five countries
mxbox <- reactive({
    filter(combo, year >= input$yearRange[1], year <= input$yearRange[2]) %>%  
    group_by(country_or_area) %>% 
    summarise(., geothermal = sum(geothermal), hydro = sum(hydro), nuclear_electricity = sum(nuclear_electricity), 
               solar_electricity = sum(solar_electricity), tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
               wind_electricity = sum(wind_electricity), Total_Consumption = sum(Total_Consumption), year = first(year),
              alpha.3 = first(alpha.3), unit = first(unit)) %>% 
    ungroup(.) %>% 
    select_(.,.dots = c('year', 'alpha.3', 'country_or_area', 'unit', input$energy, 'Total_Consumption')) %>%
    mutate(., prop = round(rowSums(select_(., .dots = input$energy))/Total_Consumption*100, 2))%>% 
    arrange(., desc(prop)) %>% 
    top_n(.,5) %>% 
    select(.,Country = country_or_area, Renewables = prop)
  })
  

output$maxBox <- renderInfoBox({
    infoBox('Top 5 Countries: ',HTML(paste0(unlist(mxbox()[1,1]),' - ', mxbox()[1,2], "%",br()),
            paste0(unlist(mxbox()[2,1]),' - ', mxbox()[2,2], "%",br()),
            paste0(unlist(mxbox()[3,1]),' - ', mxbox()[3,2], "%",br()),
            paste0(unlist(mxbox()[4,1]),' - ', mxbox()[4,2], "%",br()),
            paste0(unlist(mxbox()[5,1]),' - ', mxbox()[5,2], "%",br())),
            icon = icon("hand-o-up"), color = 'green', fill = T)
  })
   




#bottom five countries
mnbox <- reactive({
    filter(combo, year >= input$yearRange[1], year <= input$yearRange[2]) %>%  
       group_by(country_or_area) %>% 
       summarise(., geothermal = sum(geothermal), hydro = sum(hydro), nuclear_electricity = sum(nuclear_electricity), 
                 solar_electricity = sum(solar_electricity), tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
                 wind_electricity = sum(wind_electricity), Total_Consumption = sum(Total_Consumption), year = first(year),
                 alpha.3 = first(alpha.3), unit = first(unit)) %>% 
       ungroup(.) %>% 
       select_(.,.dots = c('year', 'alpha.3', 'country_or_area', 'unit', input$energy, 'Total_Consumption')) %>%
       mutate(., prop = round(rowSums(select_(., .dots = input$energy))/Total_Consumption*100, 2))%>% 
       arrange(., desc(prop)) %>% 
       top_n(.,-5) %>% 
       select(.,Country = country_or_area, Renewables = prop)
   })
   
   
output$minBox <- renderInfoBox({
    infoBox('Bottom 5 Countries: ',HTML(paste0(unlist(mnbox()[1,1]),' - ', mnbox()[1,2], "%",br()),
                                      paste0(unlist(mnbox()[2,1]),' - ', mnbox()[2,2], "%",br()),
                                      paste0(unlist(mnbox()[3,1]),' - ', mnbox()[3,2], "%",br()),
                                      paste0(unlist(mnbox()[4,1]),' - ', mnbox()[4,2], "%",br()),
                                      paste0(unlist(mnbox()[5,1]),' - ', mnbox()[5,2], "%",br())),
             icon = icon("hand-o-down"),color = 'red', fill = T)
   })
  


   
   
#avg. between countries   
combo_avg <- reactive({
    filter(combo, year >= input$yearRange[1], year <= input$yearRange[2]) %>% 
       filter(.,country_or_area == input$country1 | country_or_area == input$country2 ) %>% 
       group_by(.,  country_or_area) %>% 
       summarise(., geothermal = sum(geothermal), hydro = sum(hydro), nuclear_electricity = sum(nuclear_electricity), 
                 solar_electricity = sum(solar_electricity), tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
                 wind_electricity = sum(wind_electricity), Total_Consumption = sum(Total_Consumption), year = first(year),
                 alpha.3 = first(alpha.3), unit = first(unit)) %>% 
       ungroup(.) %>% 
       select_(.,.dots = c('year', 'alpha.3', 'country_or_area', 'unit', input$energy, 'Total_Consumption')) %>% 
       mutate(., prop = round(rowSums(select_(., .dots = input$energy))/Total_Consumption*100, 2)) %>% 
       select(.,country_or_area, prop)
       
   }) 
  
   
   
output$avgBox <- renderInfoBox({
    infoBox('Avg. Proportion: ', HTML(paste0(unlist(combo_avg()[1,1]),' - ', combo_avg()[1,2], "%",br()),
                                     paste0(unlist(combo_avg()[2,1]),' - ', combo_avg()[2,2], "%",br())),
            
            icon = icon("percent"),color = 'purple', fill = T)
   
   
   })
  
}
