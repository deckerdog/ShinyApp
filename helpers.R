library(tidyverse)
library(plotly)


#read in data from https://www.kaggle.com/unitednations/international-energy-statistics
energy_data <-  read.csv(paste0(getwd(),'/Renewable_Data/all_energy_statistics.csv'))

energy_data <- droplevels(energy_data)


#filter for renewable electricity production sources
eg_types <- distinct(energy_data,commodity_transaction) %>% 
  filter(.,str_detect(commodity_transaction, 'Electricity') & str_detect(commodity_transaction, 'production') &
           (str_detect(commodity_transaction, 'solar')| str_detect(commodity_transaction, 'wind')|
              str_detect(commodity_transaction, 'geothermal')| str_detect(commodity_transaction, 'nuclear')|
              str_detect(commodity_transaction, 'hydro')| str_detect(commodity_transaction, 'tide')|
              str_detect(commodity_transaction, 'wave')|
              str_detect(commodity_transaction, 'marine')))

ed_renew <- filter(energy_data, commodity_transaction %in% eg_types$commodity_transaction, year > 1993)
ed_renew <-  droplevels(ed_renew)
distinct(ed_renew, category)


#aggregate energy sources by year and country
energy_data <- select(ed_renew, - quantity_footnotes) %>% 
  mutate(.,kWh = quantity) %>% 
  spread(category,kWh) %>% 
  replace(., is.na(.),0) %>% 
  mutate(., total = geothermal + hydro + nuclear_electricity + solar_electricity + 
           tide_wave_and_ocean_electricity + wind_electricity) %>% 
  select(., -quantity, -commodity_transaction) %>% 
  group_by(., year, country_or_area) %>% 
  summarise(.,
           unit = first(unit),
           geothermal = sum(geothermal),
           hydro = sum(hydro),
           nuclear_electricity = sum(nuclear_electricity),
           solar_electricity = sum(solar_electricity),
           tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
           wind_electricity = sum(wind_electricity),
           total = sum(total)) %>% 
  ungroup(.)



levels(energy_data$unit) <- "Kilowatt-hours, Million"

write.csv(energy_data, file = "Renewable_Stats.csv", row.names = F)





#label locations for plotly interface------------------------------------------------------------------------------------


#read in ISO-3 codes for renewable map
energy_data <-  read.csv('./Renewable_Stats.csv')

enrg_countries <- distinct(energy_data, country_or_area)

codes = read.csv('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv')


#initial code match
add_iso = merge(enrg_countries, codes, by.x = 'country_or_area', by.y = 'name', all.x = T) %>% 
  select(.,country_or_area, alpha.3)



#export for manual entry for unlisted labels
missing_country <- filter(add_iso,is.na(alpha.3))
write.csv(missing_country, file = "missing_country_toclean.csv")

codes_final <- read.csv(paste0(getwd(),'/missing_country.csv'))
codes_final <- filter(codes_final,!is.na(alpha.3)) %>% 
  select(.,country_or_area, alpha.3)


#merge for total list
geo <- filter(add_iso, !is.na(alpha.3))
geos = rbind(geo,codes_final)
geos <- droplevels(geos)



#write to file for Shiny interface
renewables = merge(energy_data,geos, by.x = 'country_or_area', by.y = 'country_or_area', all.x = T) %>% 
  filter(.,!is.na(alpha.3))
renewables <- droplevels(renewables)

write.csv(renewables, file = "./ShinyAttempt4/Final_Renewable_Stats.csv", row.names = F)


energy_data <-  read.csv('./ShinyAttempt4/Final_Renewable_Stats.csv')





#consumption map filter---------------------------------------------------------------------------------------

library(tidyverse)
library(plotly)


#Filter for consumptions data
edc <-  read.csv(paste0(getwd(),'/Renewable_Data/all_energy_statistics.csv')) %>%
  filter(.,year > 1993)
edc <- droplevels(edc)

elec_con <- filter(edc, str_detect(commodity_transaction, "consumption"), str_detect(commodity_transaction, "Electric"))
x = distinct(elec_con, commodity_transaction)



#aggregate consumption by year and country
en_con <-  filter(edc, commodity_transaction %in% x$commodity_transaction) %>% 
  group_by(.,year,country_or_area) %>% 
  summarise(.,
            Total_Consumption = sum(quantity), 
            unit = first(unit)) %>% 
  ungroup(.)

en_con = droplevels(en_con)
write.csv(en_con, file = './ShinyAttempt2/Data/Consumption_Stats.csv', row.names = F)





#--------------------------------------------------------------------------------
#consumption locations


#read in ISO-3 codes for consumption map
geo_c = distinct(en_con, country_or_area)
geo_c = droplevels(geo_c)
codes_c = read.csv('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv')


consumptions_in_renew = merge(geo_c, geos, by.x = 'country_or_area', by.y = 'country_or_area', all.x = T)
consumptions_in_renew = droplevels(consumptions_in_renew)


#initial code match
cons_na = filter(consumptions_in_renew, is.na(alpha.3))
cons_na = merge(cons_na, codes_c, by.x = 'country_or_area', by.y = 'name', all.x = T) %>% 
  select(., country_or_area, alpha.3 = alpha.3.y)
cons_na = droplevels(cons_na)


geo_in_con = filter(consumptions_in_renew, !is.na(alpha.3)) %>% 
  rbind(.,cons_na)
geo_in_con = droplevels(geo_in_con)



#export for manual entry for unlisted labels
missing_c = filter(geo_in_con, is.na(alpha.3))
missing_c = droplevels(missing_c)
write.csv(missing_c, file = 'missing_country_C_toclean.csv', row.names = F)

codes_c_final = read.csv('./missing_country_c.csv')

geos_c <- filter(geo_in_con, !is.na(alpha.3)) %>% 
  rbind(.,codes_c_final) %>% 
  filter(., !is.na(alpha.3))
geos_c <- droplevels(geos_c)


#write to file for Shiny interface
cons_data <- read.csv(paste0(getwd(),'./ShinyAttempt2/Data/Consumption_Stats.csv'))

consumptions = merge(cons_data, geos_c, by.x = 'country_or_area', by.y = 'country_or_area', all.x = T) %>% 
  filter(.,!is.na(alpha.3))
consumptions <- droplevels(consumptions)

write.csv(consumptions, file = "./ShinyAttempt4/Final_Consumption_Stats.csv", row.names = F)

#----------------------------------------------------------------------------------------------------

#proportional energy by year

energy_data <-  read.csv('./ShinyAttempt4/Final_Renewable_Stats.csv')
consumption_data <- read.csv('./ShinyAttempt4/Final_Consumption_Stats.csv')

combo = merge(energy_data, consumption_data, by = c('year', 'alpha.3'), all.x = T) %>% 
  group_by(., year, alpha.3) %>% 
  summarise(.,
            country_or_area = first(country_or_area.x),
            unit = first(unit.x),
            geothermal = sum(geothermal),
            hydro = sum(hydro),
            nuclear_electricity = sum(nuclear_electricity),
            solar_electricity = sum(solar_electricity),
            tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
            wind_electricity = sum(wind_electricity),
            Total_RProd = sum(total),
            Total_Consumption = sum(Total_Consumption)) %>% 
  ungroup(.)

write.csv(combo, file = "./ShinyAttempt5/Final_Combined_Stats.csv", row.names = F)

#End of cleaning-------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------


  
  
  
  

-----------------------------------------------------------------------------------------------------------------
#Scratch---------------------------------------------------------------------------------------------------------

graph_cols <- c('geothermal', 'hydro', 'nuclear_electricity',
  'solar_electricity', 'tide_wave_and_ocean_electricity',
  'wind_electricity')


combo_filt <-
  filter(combo, year >= 1994, year <= 2014) %>% 
    filter(.,country_or_area == 'Afghanistan' | country_or_area == 'United States' ) %>% 
    select_(.,.dots = c('year', 'alpha.3', 'country_or_area', 'unit', graph_cols, 'Total_Consumption')) %>% 
    mutate(., prop = rowSums(select_(., .dots = graph_cols))/Total_Consumption)


g <- ggplot(combo_filt, aes(x = year, y = prop, colour = country_or_area)) +
  geom_line() + geom_point() + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.line = element_line(colour = "azure4", 
        linetype = "solid"), panel.grid.major = element_line(colour = "darkseagreen3"), 
    panel.grid.minor = element_line(colour = "darkseagreen3"), 
    plot.title = element_text(size = 18, 
        hjust = 0.5), panel.background = element_rect(fill = "azure2"), 
    legend.key = element_rect(fill = "white"), 
    legend.background = element_rect(fill = "white")) +labs(title = "Renewable Electricity as a proportion of Consumption", 
    x = "Year", y = "%", colour = "Country")


g

#-------------------------------------------------------------------------
mxbox <- 
  filter(combo, year >= 1994, year <= 2014) %>%  
    group_by(country_or_area) %>% 
    summarise(., geothermal = sum(geothermal), hydro = sum(hydro), nuclear_electricity = sum(nuclear_electricity), 
              solar_electricity = sum(solar_electricity), tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
              wind_electricity = sum(wind_electricity), Total_Consumption = sum(Total_Consumption), year = first(year),
              alpha.3 = first(alpha.3), unit = first(unit)) %>% 
    ungroup(.) %>% 
    select_(.,.dots = c('year', 'alpha.3', 'country_or_area', 'unit', c('geothermal', 'hydro', 'nuclear_electricity',
                                                                        'solar_electricity', 'tide_wave_and_ocean_electricity',
                                                                        'wind_electricity'), 'Total_Consumption')) %>%
    mutate(., prop = round(rowSums(select_(., .dots = c('geothermal', 'hydro', 'nuclear_electricity',
                                                        'solar_electricity', 'tide_wave_and_ocean_electricity',
                                                        'wind_electricity')))/Total_Consumption*100, 2))%>% 
    arrange(., desc(prop)) %>% 
  top_n(.,-5) %>% 
  select(.,Country = country_or_area, Renewables = prop)

mxbox = droplevels(mxbox)

paste0(mxbox[1,1],' - ', mxbox[1,2], "%",paste0(unlist(mxbox[2,1]),' - ', mxbox[2,2]), "%",
                                   paste0(unlist(mxbox[3,1]),' - ', mxbox[3,2], "%"),
                                   paste0(unlist(mxbox[4,1]),' - ', mxbox[4,2], "%"),
                                   paste0(unlist(mxbox[5,1]),' - ', mxbox[5,2], "%"))


levels(mxbox$Country)


#----------------------------------------------------------------------------------------------------
nums = c(1,700,30000)
gs = c('geothermal', 'hydro', 'nuclear_electricity',
       'solar_electricity', 'tide_wave_and_ocean_electricity',
       'wind_electricity')
length(nums)

gc <-  c('geothermal', 'hydro','solar_electricity')


gs <- gs[gs %in% gc]
nums2 = log10(nums)

ls <- c()

for (i in 1:(length(gs)-1)) {
ls[i] = paste(gs[i], nums2[i],'%', '<br>')
}
ls <- paste(ls, paste(gs[-1], nums2[-1],'%'))


paste("Geothermal", 1,'%', " Hydro", 2,'%', "<br>",
        "Nuclear", 3,'%', " Solar", 4,'%',"<br>",
        "Marine", 5,'%', " Wind", 6)




#-----------------------------------------------------------------

energy_data_sums <- filter(energy_data, year >= 1994, year <= 2014) %>% 
  group_by(., alpha.3) %>% 
  summarise(., geothermal = sum(geothermal), hydro = sum(hydro), nuclear_electricity = sum(nuclear_electricity), 
            solar_electricity = sum(solar_electricity), tide_wave_and_ocean_electricity = sum(tide_wave_and_ocean_electricity),
            wind_electricity = sum(wind_electricity), total = sum(total), country_or_area = first(country_or_area)) %>% 
  ungroup(.) %>% 
  mutate(., select_total = log10(rowSums(select_(., .dots = gs))+1))

gs = c('geothermal', 'hydro', 'nuclear_electricity',
       'solar_electricity', 'tide_wave_and_ocean_electricity',
       'wind_electricity')
gl = c('Geothermal', 'Hydro','Nuclear', 'Solar', 'Marine','Wind')

nums <- select_(energy_data_sums,.dots = gs) %>% 
  mutate(., pc = rowSums(select_(., .dots = gs)))

jkl = data.frame(gs,gl)
jkl <- filter(jkl, gs %in% names(nums))
names(nums) = jkl$gl


ls <- data.frame()
for (i in 1:nrow(nums)) { 
  for (j in 1:(ncol(nums)-1)) {
    ls[i,j] <-paste0(names(nums)[j],'-', round(nums[i,j]/nums[i, ncol(nums)]*100, 2),'%', '\n')
    }
}

lf = do.call("paste",ls)
for (i in 1:length(lf)) {
  lf[i] <- paste0(energy_data_sums$country_or_area[i], '\n', lf[i])

}
ls$final <- lf
 


nums$test = ls$hoverc

library(devtools)



jkl = data.frame(gs,gl)
jkl <- filter(jkl, gs %in% names(nums))
names(nums) = jkl$gl
