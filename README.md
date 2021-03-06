# ShinyApp
ShinyApp
https://decker.shinyapps.io/shinyproj1/



The earth’s ever-growing population demands a proportionally increasing supply of available electricity. Modern climate science has revealed how damaging historical practices of energy production and consumption have been to the global ecosystem. These competing constraints have led to the political and technological advancement of renewable energy sources as fuel alternatives. But how are these changes taking place? Which technologies are having the greatest impact on international energy portfolios? At what pace and in which countries? The purpose of this tool is to investigate these questions through visualization of production and consumption trends over the past 20 years.



The first tab allows us to look at production and consumption by country, measured in millions of Kilowatt-Hours.
![image](https://user-images.githubusercontent.com/43554810/49058253-384f9a80-f1d1-11e8-9a19-b8fee8e38b22.png)


The first dropdown menu option, ‘Production’, generates a choropleth map of the globe shaded according to renewable energy production. Countries with low renewable production have lighter shading while countries with higher renewable production have correspondingly darker shading. The user can decide a date range and a set of renewable technologies they want to visualize and the map will dynamically filter its output to meet these constraints.





Mousing over a particular country will bring up a hovering menu breaking down that country’s renewable energy production portfolio. This menu includes gross electricity production (on the log10 scale) for a given set of filters as well as a breakdown of how much each chosen energy source contributed to the gross under these parameters.
![screenshot 6](https://user-images.githubusercontent.com/43554810/49058013-215c7880-f1d0-11e8-9c76-a69559574d1d.png)



The second dropdown option, ‘Consumption’, generates a similar choropleth map of global electricity consumption over a given date range. Comparing the two maps gives a visual representation of how renewable energy sources have grown relative to the growth in demand. In both cases, the gross energy measurements have been scaled by log10 to account for a wide disparity between small and large countries in both energy consumption and production.
![image](https://user-images.githubusercontent.com/43554810/49058503-4520be00-f1d2-11e8-8f73-19b6ea57e869.png)



The second tab of the web tool allows us to directly compare two country’s year-to-year renewable energy production as a proportion of their total consumption. As with the ‘Production’ map, users have the ability to filter this output according to a certain year range and set of renewable technologies. This dynamic graph is supplemented by summary boxes stating the average proportion of renewables for the two chosen countries, as a well as a ranking for the top and bottom five countries under the given filter. This tab allows the user to look at individual countries more granularly and further investigate any trends perceived in the visualized data.
![image](https://user-images.githubusercontent.com/43554810/49058533-6386b980-f1d2-11e8-89f4-24d7b8d89379.png)




In its current state, this project is best suited for high-level exploration of international renewable energy portfolio. In order to better visualize an expanded data set,  and do so with a higher level of rigor, the tool will be modified to incorporate a clustering algorithm for identifying countries with similar energy portfolios.
