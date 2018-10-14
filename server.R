


function(input, output) {
  
 choice <- reactive({input$chlomaps})

  output$both <-
    renderPlotly({
      if (choice() == 'Production') {
  energy_data_sums <- group_by(energy_data, alpha.3) %>% 
      filter(., year >= input$yearRange[1], year <= input$yearRange[2]) %>% 
      summarise(., geothermal = sum(geothermal), hydro = sum(hydro), nuclear_electricity = sum(nuclear_electricity), 
                solar_electricity = sum(solar_electricity), tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
                wind_electricity = sum(wind_electricity), total = sum(total), country_or_area = first(country_or_area))
    
    
    energy_data_sums$hover <- with(energy_data_sums, paste(country_or_area, '<br>', "Geothermal", geothermal, "Hydro", hydro, "<br>",
                                                           "Nuclear", nuclear_electricity, "Solar", solar_electricity,
                                                           "<br>", "Marine", tide_wave_and_ocean_electricity, "Wind", wind_electricity))
    
    
    l <- list(color = toRGB("white"), width = 2)
    
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(energy_data_sums) %>%
      add_trace(
        z = ~total, text = ~hover, locations = ~alpha.3,
        color = ~total, colors = 'Greens'
      ) %>%
      colorbar(title = "kWh - Million") %>%
      layout(
        title = 'Global Renewable Electricity Production',
        geo = g
      )
  
  

  
  } else {
    cons_data_sums <- group_by(consumption_data, alpha.3) %>% 
      filter(.,year >= input$yearRange[1], year <= input$yearRange[2]) %>% 
      summarise(., total = sum(Total_Consumption), country_or_area = first(country_or_area))
    
    
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
      colorbar(title = "kWh - Million") %>%
      layout(
        title = 'Global Electricity Consumption',
        geo = g
      )}
  })
  
  
   combo_filt <- reactive({
    filter(combo, year >= input$yearRange[1], year <= input$yearRange[2]) %>% 
    filter(.,country_or_area == input$country1 | country_or_area == input$country2 ) %>% 
    select_(.,.dots = c('year', 'alpha.3', 'country_or_area', 'unit', input$energy, 'Total_Consumption')) %>% 
    mutate(., prop = rowSums(select_(., .dots = input$energy))/Total_Consumption)
    })
    
    
  output$comp <- renderPlot(ggplot(combo_filt(), aes(x = year, y = prop, colour = country_or_area))+
    geom_line() + geom_point())
  
}
