#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pdftools)
library(tidyverse)
library(shiny)
library(readxl)
library(dplyr)
library(data.table)
library(readr)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NZ Wine (Hope this is okay, yes I will come to study hall to sort out my data))"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Year",
                     "Year:",
                     min = 2009,
                     max = 2018,
                     value = 2009)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     #reading the xl file
     readin <- read_excel("Summary_of_NZ_Wines.xlsx")
     #swapping axis
     edit_data <- as.data.frame(t(readin))
     #fixing column names 
     names(edit_data) <- lapply(edit_data[1, ], as.character)
     #removing unneeded row 
     excel_nzw_data <- edit_data[-c(1), ] 
     
     #make row headers year veriable
     setDT(excel_nzw_data , keep.rownames = TRUE)
     
     
     #making trail graph 
     Graph <- excel_nzw_data %>% 
       select(rn, "Number of wineries") 
     
     ggplot(Graph, aes(x = rn, y = "Number of wineries")) + geom_point()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

