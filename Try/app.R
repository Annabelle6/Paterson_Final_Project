#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
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
     
     #reading the xl file
     readin <- read_excel("Summary_of_NZ_Wines.xlsx")
     #swapping axis
     edit_data <- as.data.frame(t(readin))
     #fixing column names 
     names(edit_data) <- lapply(edit_data[1, ], as.character)
     #removing unneeded row 
     excel_nzw_data <- edit_data[-c(1), ] 
     setDT(excel_nzw_data , keep.rownames = TRUE)
     
     
     #making trail graph 
     Graph <- excel_nzw_data %>% 
       select(rn, "Number of wineries") 
     
     ggplot(Graph, aes(x = "rn", y = "Number of wineries")) + geom_smooth()


        
        
# Run the application 
shinyApp(ui = ui, server = server)

