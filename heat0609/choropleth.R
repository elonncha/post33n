library(tidyverse)
library(shiny)
library(leaflet)
library(RColorBrewer)

#' -----------------------------------------------------------------------------
#' @color
# display.brewer.all()
reds = colorBin("Reds", domain = project.heat$extra_heat_days, bins = c(0,1,2,3,30,45,60,75,90,Inf))
#' -----------------------------------------------------------------------------




#' -----------------------------------------------------------------------------
#' @data
project.heat = read_csv('data/cleaned/project-heat-ga.csv')
project.heat = project.heat %>% mutate(COUNTYFP = as.character(COUNTYFP))

ga.county.shp = st_read('data/cleaned/tl_2020_ga_county.shp')
#' -----------------------------------------------------------------------------



ui = bootstrapPage(tags$style(type = "text/css", "html, body, .leaflet {width:100%; height:100%}"),
                   leafletOutput("map", width = "100%", height = "100%"),
                   absolutePanel(bottom = 10, right = 0, draggable = F,
                                 # slider title, step increments, and ticks
                                 selectInput("period", "Projection Period",
                                             c(2045, 2065, 2099)
                                 ),
                                 selectInput("scenario", "Emission Scenario",
                                            c("Low - RCP 4.5", "High - RCP 8.5")
                                 )
                   )
     )




server = function(input, output, session) {
  # get filtered data from input
  filteredData = reactive({
    ga.county.shp %>% left_join(project.heat %>% 
                                  filter(period == input$period, 
                                         scenario == tolower(strsplit(input$scenario, " ")[[1]][1])
                                  ), 
                                by = 'COUNTYFP')
    
  })
  
  # initiate a leaflet map 
  output$map = renderLeaflet({
    leaflet(project.heat) %>% addTiles() %>%
      setView(lat=33, lng=-80, zoom=9) %>%
      fitBounds(-85, 30, -80, 36) 
  })
  
  observe({
    # retrieve new data from reactive
    newData = filteredData()
    # update labels
    filteredLabel = sprintf(
        "<strong>%s</strong><br/>%d More Extra Extreme Heat Days",
        newData$COUNTYNAME.y, newData$extra_heat_days) %>% lapply(htmltools::HTML)
    
    # draw the map
    leafletProxy("map", data = newData) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~reds(extra_heat_days),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  
                  label = filteredLabel,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })
    
}




shinyApp(ui, server)
