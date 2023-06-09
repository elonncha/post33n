extrafont::font_import()
library(ggplot2)
library(tidyverse)
library(plotly)
library(hrbrthemes)
library(lubridate)
source('R/loading.R')

#' -----------------------------------------------------------------------------
#' @data
historic.heat.GAavg = historic.heat %>% 
  group_by(year) %>% 
  summarise(avg_heat_days = mean(heat_days))

historic.heat.ATLMavg = historic.heat %>% 
  filter(COUNTYFP %in% COUNTYFP.ATLM) %>%
  group_by(year) %>% 
  summarise(avg_heat_days = mean(heat_days))

historic.heat.ARCavg = historic.heat %>% 
  filter(COUNTYFP %in% COUNTYFP.ARC) %>%
  group_by(year) %>% 
  summarise(avg_heat_days = mean(heat_days))
#' -----------------------------------------------------------------------------


#' -----------------------------------------------------------------------------
#' @colorpicker
BROWN = "#AD8C97"
BROWN_DARKER = "#D19A19"
GREEN = "#2FC1D3"
BLUE = "#076FA1"
GREY = "#C7C9CB"
GREY_DARKER = "#5C5B5D"
RED = "#E3120B"
#' -----------------------------------------------------------------------------



#spline_df   = as.data.frame(spline(historic.heat.GAavg$year, historic.heat.GAavg$avg_heat_days, 
#                                   n = 2000, method = "nat"))
#spline_df   = spline_df[2:1999, ]
#grad_df = data.frame(xintercept = seq(1979, 2021, length.out = 200), 
#                      alpha = seq(0.6, 0.3, length.out = 200))


#' -----------------------------------------------------------------------------
#' @PLOT
p = ggplot() + 
      # gradient area fill for GA trend lines
      #geom_area(data = spline_df, aes(x, y), fill = BROWN_DARKER, alpha = 0.15) + 
      #geom_vline(data = grad_df, aes(xintercept = xintercept, alpha = alpha), 
      #          size = 2.5, colour = "white") +
      # trend lines
      geom_line(data = historic.heat.GAavg, aes(x = year, y = avg_heat_days), 
                color=BROWN_DARKER, alpha = 0.3, size = 0.5) +
      geom_line(data = historic.heat.ARCavg, aes(x = year, y = avg_heat_days), 
                color=BLUE, alpha = 0.3, size = 0.5) +
      # data points
      geom_point(data = historic.heat.GAavg, aes(x = year, y = avg_heat_days),
                 fill = BROWN_DARKER, color = BROWN_DARKER, alpha = 0.4, size = 1.5, pch = 21
      ) + 
      geom_point(data = historic.heat.ARCavg, aes(x = year, y = avg_heat_days),
                 fill = BLUE, color = BROWN_DARKER, alpha = 0.4, size = 1.5, pch = 21
      ) + 
      # LOESS fitted curves
      geom_smooth(data = historic.heat.GAavg, aes(x = year, y = avg_heat_days), 
                  method = 'loess', span = 25, size = 3, 
                  color=BROWN_DARKER, se=F) + 
      geom_smooth(data = historic.heat.ARCavg, aes(x = year, y = avg_heat_days), 
                  method = 'loess', span = 25, size = 3, 
                  color=BLUE, se=F)

p = p + 
      # legend lines
      geom_line(data = data.frame(x = c(1979, 1982), y = 90), aes(x,y),
                size = 3, color=BROWN_DARKER) + 
      geom_line(data = data.frame(x = c(1979, 1982), y = 85), aes(x,y),
                size = 3, color=BLUE) + 
      # two stress points
      geom_point(data = data.frame(x = 2021, y = 67), aes(x,y),
                 fill = BROWN_DARKER, size = 7, pch = 21,
                 color = "white", stroke = 3
      ) + 
      geom_point(data = data.frame(x = 2021, y = 47.5), aes(x,y),
                 fill = BLUE, size = 7, pch = 21,
                 color = "white", stroke = 3
      ) 

  
p = p + 
  # adjust x and y axis
  scale_x_continuous(
    limits = c(1978, 2022),
    expand = c(0, 0),
    breaks = seq(1980, 2020, 10),  
    labels = c("1980", "1990", "2000", "2010", "2020")
  ) + 
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, by = 10), 
    expand = c(0, 0)
  ) + 
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Remove all grid lines
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    axis.title = element_blank(),
    # Only the bottom line of the vertical axis is painted in black
    axis.line.x.bottom = element_line(color = "black"),
    # But customize labels for the horizontal axis
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 16),
    
    plot.margin=unit(c(1,2,1,0), "cm"),
    panel.margin=unit(c(1,2,1,0), "cm")
  )

p = p + 
  # horizontal tick
  geom_text(
    data = data.frame(x = 2022, y = seq(10, 100, by = 10)),
    aes(x, y, label = y),
    hjust = 1, # Align to the right
    vjust = 0, # Align to the bottom
    nudge_y = 32 * 0.01, # The pad is equal to 1% of the vertical range (32 - 0)
    size = 6,
    alpha = 0.2
  ) + 
  # title
  geom_text(
    data = data.frame(x = 1979, y = 105, t = "Number of Extreme Heat Days (County Average, 1979-2021)"),
    aes(x, y, label = t), fontface = "bold",
    hjust = 0, # Align to the left
    vjust = 1, size = 7
  ) + 
  # legend text
  geom_text(
    data = data.frame(x = 1982.3, y = 89, t = "Georgia"),
    aes(x, y, label = t),
    hjust = 0, # Align to the left
    vjust = 0, size = 6
  ) + 
  geom_text(
    data = data.frame(x = 1982.3, y = 84, t = "ARC 11-County Region"),
    aes(x, y, label = t),
    hjust = 0, # Align to the left
    vjust = 0, size = 6
  ) +
  # stress point annotation 
  geom_text(
    data = data.frame(x = 2021.5, y = 67, t = "67"),
    aes(x, y, label = t), fontface = "bold",
    hjust = 1.5, vjust = -0.5, size = 9
  ) + 
  geom_text(
    data = data.frame(x = 2021.5, y = 47.5, t = "48"),
    aes(x, y, label = t), fontface = "bold",
    hjust = 1.5, vjust = -0.5, size = 9
  ) + 
  labs(caption = "Note: The trend curve is fitted using LOESS regression for each region.\nData Source:  CDC National Environmental Public Health Tracking Network") +
  theme(plot.caption = element_text(hjust=0))
  
p  
