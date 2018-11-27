# ShinyApp
ShinyApp
https://decker.shinyapps.io/Renewables/



The earth’s ever-growing population demands a proportionally increasing supply of available electricity. Conversely, modern climate science has revealed how damaging historical practices of energy production and consumption have been to the global ecosystem. Navigating these competing constraints has led to the political and technological advancement of renewable energy sources as competitive fuel alternatives. But how are these changes taking place? Which technologies are having the greatest impact on international energy portfolios? At what pace and in which countries? The purpose of this tool is to investigate these questions through visualization of production and consumption trends over the past 20 years.



The first tab allows us to look at production and consumption by country, measured in millions of Kilowatt-Hours.
![screenshot 11](https://user-images.githubusercontent.com/43554810/49057873-88c5f880-f1cf-11e8-8838-5700d773c897.png)


The first dropdown menu option, ‘Production’, generates a choropleth map of the globe shaded according to renewable energy production. Countries with low renewable production have lighter shading while countries with higher renewable production have correspondingly darker shading. The user can decide a date range and a set of renewable technologies they want to visualize and the map will dynamically filter its output to meet these constraints.





Mousing over a particular country will bring up a hovering menu breaking down that country’s renewable energy production portfolio. This menu includes gross electricity production for a given set of filters as well as a breakdown of how much each chosen energy source contributed to the gross under these parameters.




The second dropdown option, ‘Consumption’, generates a similar choropleth map of global electricity consumption over a given date range. Comparing the two maps gives a visual representation of how renewable energy sources have grown relative to the growth in demand. In both cases, the gross energy measurements have been scaled by log10 to account for a wide disparity between small and large countries in both energy consumption and production.




The second tab of the web tool allows us to directly compare two country’s year-to-year renewable energy production as a proportion of their total consumption. As with the ‘Production’ map, users have the ability to filter this output according to a certain year range and set of renewable technologies. This dynamic graph is supplemented by summary boxes stating the average proportion of renewables for the two chosen countries, as a well as a ranking for the top and bottom five countries under the given filter. This tab allows the user to look at individually countries more granularly and further investigate any trends perceived in the visualized data.





In its current state, this project is best suited for high-level exploration of our international renewable energy portfolio. This could be expanded to include a wider range of energy sources for the user to choose from, along with an option to un-scale the data as a means of trading visual aesthetic for greater interpretability. In order to better visualize an expanded data set,  and do so with a higher level of rigor, the tool could be modified to incorporate a clustering algorithm for identifying countries with similar energy portfolios.
