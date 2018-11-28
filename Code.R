library(tidyverse)
library(shiny)
library(readxl)
library(dplyr)
install.packages("data.table")
library(data.table)
library(readr)
library(janitor)
install.packages("norm")
library(norm)
library(leaflet)

#Making wineries region data 

#reading the xl file
readin <- read_excel("Wineries_by_region.xlsx")

#swapping axis
edit_data <- as.data.frame(t(readin))

#fixing column names 
names(edit_data) <- lapply(edit_data[1, ], as.character)

#removing unneeded row 
wineries_region_data <- edit_data[-c(1), ]

#make the year a col
setDT(wineries_region_data , keep.rownames = TRUE)

#rename year col
colnames(wineries_region_data)[colnames(wineries_region_data)=="rn"] <- "year"

#clean names
wineries_region_data <- wineries_region_data %>% 
  clean_names()

----------------------------------------------------
  
#making nz region data 
#creating a dataframe to join with the geojson
  
nz_regions <- readRDS("nz_regions/nz_region.rds") 

nz_regions <- nz_regions %>% 
  select(Area) %>% 
  group_by(Area) %>% 
  count(Area) %>% 
  select(Area) 

#nz_regions matchches the geojson file that I want to use for my map
#I want to join the two files to add the wine data to the geojson file 

---------------------------------------------------
  
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
           case_when(order == "Northland" ~ "09", 
                     order == "Auckland" ~ "01", 
                     order == "Waikato" ~ "14", 
                     order == "Waikato/Bay of Plenty" ~ "02", 
                     order == "Gisborne"  ~ "04", 
                     order == "Hawke's Bay" ~ "05", 
                     order == "Wairarapa" ~ "15", 
                     order == "Nelson" ~ "08", 
                     order == "Marlborough" ~ "07", 
                     order == "Canterbury/Waipara" ~ "03", 
                     order == "Central Otago" ~ "10", 
                     order == "Waitaki Valley" ~ "11",
                     order == "Manawatu-Wanganui Region" ~ "06",
                     order == "Taranaki Region" ~ "12",
                     order == "Tasman Region" ~ "13",
                     order == "West Coast Region" ~ "16")) %>% 
  
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

---------------------------------------------------------