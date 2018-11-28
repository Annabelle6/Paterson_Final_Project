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
  
