library(shiny)
library(plotly)
library(shinythemes)


data <- readRDS("Nz_wine_map_data")

# Define UI for Education application
ui <- navbarPage(
  "New Zealand Wineries",
  
  
  # Add info panel
  tabPanel("Data Information",
           fluidPage(
             titlePanel("Data"),
             h2("...."),
             p("....")
           )
  ),
  # Add education page with sidebar and plot
  tabPanel("2009",
           fluidPage(
             plotlyOutput("plot2009")
           )
  ),
  
  
  tabPanel("2010",
           fluidPage(
             plotlyOutput("plot2010")
           )
  ),
  
  
  tabPanel("2011",
           fluidPage(
             plotlyOutput("plot2011")
           )
  ), 
  
  
  tabPanel("2012",
           fluidPage(
             plotlyOutput("plot2012")
           )
  ), 
  
  tabPanel("2013",
           fluidPage(
             plotlyOutput("plot2013")
           )
  ), 
  
  
  tabPanel("2014",
           fluidPage(
             plotlyOutput("plot2014")
           )
  ), 
  
  
  tabPanel("2015",
           fluidPage(
             plotlyOutput("plot2015")
           )
  ), 
  
  
  tabPanel("2016",
           fluidPage(
             plotlyOutput("plot2016")
           )
  ), 
  
  
  tabPanel("2017",
           fluidPage(
             plotlyOutput("plot2017")
           )
  ), 
  
  
  tabPanel("2018",
           fluidPage(
             plotlyOutput("plot2018")
           )
  )
)



server <- function(input, output, session) {
  
  ######################### 2009
  
  output$plot2009 <- renderPlotly({
    # specify some map projection/options
    p2009 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2009)
      ) +
      theme_void()
    
    ggplotly(p2009) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  ######################### 2010
  
  output$plot2010 <- renderPlotly({
    # specify some map projection/options
    p2010 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2010)) +
      theme_void()
    
    ggplotly(p2010) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  ######################### 2011
  
  output$plot2011 <- renderPlotly({
    # specify some map projection/options
    p2011 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2011)
      ) +
      theme_void()
    
    ggplotly(p2011) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  
  ######################### 2012
  
  output$plot2012 <- renderPlotly({
    # specify some map projection/options
    p2012 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2012)
      ) +
      theme_void()
    
    ggplotly(p2012) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  
  ######################### 2013
  
  output$plot2013 <- renderPlotly({
    # specify some map projection/options
    p2013 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2013)
      ) +
      theme_void()
    
    ggplotly(p2013) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  ######################### 2014
  
  output$plot2014 <- renderPlotly({
    # specify some map projection/options
    p2014 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2014)
      ) +
      theme_void()
    
    ggplotly(p2014) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  
  ######################### 2015
  
  output$plot2015 <- renderPlotly({
    # specify some map projection/options
    p2015 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2015)
      ) +
      theme_void()
    
    ggplotly(p2015) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  
  ######################### 2016
  
  output$plot2016 <- renderPlotly({
    # specify some map projection/options
    p2016 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2016)
      ) +
      theme_void()
    
    ggplotly(p2016) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  
  ######################### 2017
  
  output$plot2017 <- renderPlotly({
    # specify some map projection/options
    p2017 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2017)
      ) +
      theme_void()
    
    ggplotly(p2017) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  
  ######################### 2018
  
  output$plot2018 <- renderPlotly({
    # specify some map projection/options
    p2018 <- ggplot(data = data) +
      geom_sf(aes(fill = Region, text = x2018)
      ) +
      theme_void()
    
    ggplotly(p2018) %>%
      highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
      )
  })
  
  
}

shinyApp(ui, server)


