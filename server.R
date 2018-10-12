

function(input, output) {
  
  output$RenewYRange <- renderPlotly({
    energy_data_sums <- group_by(energy_data, country_or_area) %>% 
      filter(.,year > input$yearRange[1], year < input$yearRange[2]) %>% 
      summarise(., geothermal = sum(geothermal), hydro = sum(hydro), nuclear_electricity = sum(nuclear_electricity), 
              solar_electricity = sum(solar_electricity), tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
              wind_electricity = sum(wind_electricity), total = sum(total), alpha.3 = first(alpha.3))
    
    
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
      colorbar(title = "kWh - Hundred-Million") %>%
      layout(
        title = 'Global Renewable Electricity Production',
        geo = g
      )
 })
}