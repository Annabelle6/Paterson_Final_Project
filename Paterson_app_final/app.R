library(shiny)
library(plotly)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("paper"),
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  
  # Application title
  titlePanel("New Zealand Wineries by Region")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    # specify some map projection/options
    p <- ggplot(data = full) +
      geom_sf(aes(fill = Region, text = x2009)
      )
    ggplotly(p) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a region to view event data" else d
  })
  
}

shinyApp(ui, server)