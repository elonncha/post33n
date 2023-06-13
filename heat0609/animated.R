library(ggplot2)
library(gganimate)
library(tidyverse)
library(RColorBrewer)
library(geojsonsf)
library(leaflet)
library(sf)
library(magick)

#' -----------------------------------------------------------------------------
#' @data
project.heat = read_csv('data/cleaned/project-heat-ga.csv')
project.heat = project.heat %>% mutate(COUNTYFP = as.character(COUNTYFP))

county.atlregion = read_csv('data/raw/Counties_Atlanta_Region.csv')
COUNTYFP.ARC = county.atlregion$GEOID10[county.atlregion$Reg_Comm == 'Atlanta Regional Commission']

ga.county.shp = st_read('data/cleaned/tl_2020_ga_county.shp')
arc.border.shp = ga.county.shp %>% filter(COUNTYFP %in% COUNTYFP.ARC) %>% bind_rows() %>% st_union()

#' -----------------------------------------------------------------------------

reds = brewer.pal(n = 9, name = 'OrRd')

drawChoropleth = function(sce, per) {
  # sce = ['low', 'high']
  # per = [2045, 2065, 2099]
  
  filteredData = ga.county.shp %>% left_join(project.heat %>% 
                                             filter(period == per, 
                                                    scenario == sce), 
                                             by = 'COUNTYFP')
  
  map = ggplot() + 
    geom_sf(data = filteredData, aes(fill = extra_heat_days), lwd = 0.2) + 
    geom_sf(data = st_union(arc.border.shp), fill = NA, colour = 'black', alpha = 0.5, lwd = 1.5) +
    geom_sf_text(data = st_union(arc.border.shp), aes(label = 'ARC \n Region'), color = 'black') + 
    theme_void()
  map = map + 
    labs(
      title = sprintf("%s Emission Scenario    |     Projection Year: %i", str_to_title(sce), per)
    ) + 
    scale_fill_stepsn(colours = reds, breaks = c(10,20,30,40,50,60,70,80), limits= c(0,110),
                      name = "Additional Extra Heat Days") + 
    theme(
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
      plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
      
      legend.position = c(0.3, 0.05),
      legend.direction = 'horizontal', 
      legend.key.size = unit(1, 'cm'), 
      legend.key.height = unit(0.5, 'cm'), 
      legend.key.width = unit(1, 'cm'), 
    )+ 
    coord_sf()
  
  ggsave(paste('output/projectheat-',sce,'-', per, '.jpeg', sep=''),
         width = 9, height = 9, units = 'in', dpi=300)
  
}



for (sce in c('low', 'high')) {
  for (per in c(2045, 2065, 2099)) {
    drawChoropleth(sce,per)
  }
}


# Read the individual maps into a data structure for use with 'magick'
imglayer.low = sapply(c(2045, 2065, 2099), function(yr) {
  image_read(paste('output/projectheat-low-', yr, '.jpeg', sep=''))
})
imglayer.high = sapply(c(2045, 2065, 2099), function(yr) {
  image_read(paste('output/projectheat-high-', yr, '.jpeg', sep=''))
})

# Generate an animated GIF with the individual maps and write to a file
gif.low = image_animate(image_join(imglayer.low), fps = 1, dispose = "previous")
gif.high =  image_animate(image_join(imglayer.high), fps = 1, dispose = "previous")
image_write(gif.low, 'output/projectheat-low.gif')
image_write(gif.high, 'output/projectheat-high.gif')
