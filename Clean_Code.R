library(tidyverse)
library(shiny)
library(readxl)
library(dplyr)
library(data.table)
library(readr)
library(janitor)
library(norm)
library(leaflet)

#Making wineries region data 

#reading the xl file
readin <- read_excel("Wineries_by_region.xlsx")

#creating a dataframe to join with the geojson
#I need to go back to my wineries_region_data because I dont need to 
#change the axis of the regions 

new_wineries_region <- readin %>% 
  clean_names() 

#the wine data has different regions then the regions file 
#because some regions don't have vinyards 
#I will need to add additional rows so they will match 

manawatu <- data.frame("Manawatu-Wanganui Region", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
names(manawatu)<- c("wineries_by_region", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017", "x2018")

taranaki <- data.frame("Taranaki Region", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
names(taranaki)<- c("wineries_by_region", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017", "x2018")

tasman <- data.frame("Tasman Region", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
names(tasman)<- c("wineries_by_region", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017", "x2018")

WestCoast <- data.frame("West Coast Region", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
names(WestCoast)<- c("wineries_by_region", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017", "x2018")

#adding the additional rows so the two data sets match 
new_wineries_region <- rbind(new_wineries_region, manawatu)
new_wineries_region <- rbind(new_wineries_region, taranaki)
new_wineries_region <- rbind(new_wineries_region, tasman)
new_wineries_region <- rbind(new_wineries_region, WestCoast)

#remove total and other 
new_wineries_region <- new_wineries_region[-c(12, 13), ]

#wine data has combined Waikato and Bay of plenty 
#I am going to split them up but keep the data the same for both 

waikato <- data.frame("Waikato", "20", "21", "17", "15", "13", "13", "12", "9", "8", "10")
names(waikato)<- c("wineries_by_region", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017", "x2018")

new_wineries_region <- rbind(new_wineries_region, waikato)

#now I want to re-order my rows 
#I am going to add a new row of numbers to get them in the right order

#creating new col to create order with regions 
new_wineries_region$order = new_wineries_region$wineries_by_region

order_new <- new_wineries_region %>% 
  mutate(order = 
           case_when(order == "Northland" ~ "01", 
                     order == "Auckland" ~ "02", 
                     order == "Waikato" ~ "03", 
                     order == "Waikato/Bay of Plenty" ~ "04", 
                     order == "Gisborne"  ~ "05", 
                     order == "Hawke's Bay" ~ "06", 
                     order == "Wairarapa" ~ "09", 
                     order == "Nelson" ~ "17", 
                     order == "Marlborough" ~ "18", 
                     order == "Canterbury/Waipara" ~ "13", 
                     order == "Central Otago" ~ "14", 
                     order == "Waitaki Valley" ~ "15",
                     order == "Manawatu-Wanganui Region" ~ "08",
                     order == "Taranaki Region" ~ "07",
                     order == "Tasman Region" ~ "16",
                     order == "West Coast Region" ~ "12")) %>% 
  
  #rearranging with the new order col
  arrange(order)

#rename the regions to match the nz_regions
name_new <- order_new %>% 
  mutate(wineries_by_region = 
           case_when(wineries_by_region == "Auckland" ~ "Auckland Region",
                     wineries_by_region == "Waikato/Bay of Plenty" ~ "Bay of Plenty Region",
                     wineries_by_region == "Canterbury/Waipara" ~ "Canterbury Region",
                     wineries_by_region == "Gisborne" ~ "Gisborne Region",
                     wineries_by_region == "Hawke's Bay" ~ "Hawke's Bay Region", 
                     wineries_by_region == "Manawatu-Wanganui Region" ~ "Manawatu-Wanganui Region",
                     wineries_by_region == "Marlborough" ~ "Marlborough Region",
                     wineries_by_region == "Nelson" ~ "Nelson Region",
                     wineries_by_region == "Northland" ~ "Northland Region",
                     wineries_by_region == "Central Otago" ~ "Otago Region",
                     wineries_by_region == "Waitaki Valley" ~ "Southland Region",
                     wineries_by_region == "Taranaki Region" ~ "Taranaki Region",
                     wineries_by_region == "Tasman Region" ~ "Tasman Region",
                     wineries_by_region == "Waikato" ~ "Waikato Region",
                     wineries_by_region == "Wairarapa" ~ "Wellington Region", 
                     wineries_by_region == "West Coast Region" ~ "West Coast Region"))

colnames(name_new)[colnames(name_new)=="order"] <- "REGC2016"
colnames(name_new)[colnames(name_new)=="wineries_by_region"] <- "REGC2016_N"

-----------------------------------------------------
library(tidyverse)
library(sf)
library(ggplot2)
nz_json <- st_read("nz_regions/nz_region.geojson")

name_new <- name_new %>% 
  select(-REGC2016)

full <- full_join(nz_json, name_new, by = "REGC2016_N")

colnames(full)[colnames(full)=="REGC2016_N"] <- "Region"

write_rds(full, "Nz_wine_map_data", compress = "none")


suppressPackageStartupMessages(library(plotly))

p <- ggplot(data = full) +
  geom_sf(aes(fill = REGC2016_N))

ggplotly(p) %>%
  highlight(
    "plotly_hover",
    selected = attrs_selected(line = list(color = "black"))
  )









