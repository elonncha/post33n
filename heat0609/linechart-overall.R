library(ggplot2)
library(tidyverse)
library(hrbrthemes)

#' -----------------------------------------------------------------------------
#' @data
suppressMessages({
  county.atlregion = read_csv('data/raw/Counties_Atlanta_Region.csv')
  historic.heat = read_csv('data/raw/historic-heat_ga_county.csv')
  
  COUNTYFP.ATLM = county.atlregion$GEOID10
  COUNTYFP.ARC = county.atlregion$GEOID10[county.atlregion$Reg_Comm == 'Atlanta Regional Commission']
  COUNTYFP.GA = unique(historic.heat$CountyFIPS)
  
  rm(county.atlregion)
  rm(historic.heat)
  
  historic.heat = read_csv('data/cleaned/historic-heat-ga.csv')
})

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
#' @styles
BROWN_DARKER = "#D19A19"
BLUE = "#076FA1"
#' -----------------------------------------------------------------------------




#' -----------------------------------------------------------------------------
#' @PLOT
p = ggplot() + 
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
                  color=BLUE, se=F) + 
      # add us average line
      geom_hline(aes(yintercept = 15), linetype = 'longdash')

p = p + 
      # legend lines
      geom_line(data = data.frame(x = c(1979, 1982), y = 90), aes(x,y),
                size = 3, color=BROWN_DARKER) + 
      geom_line(data = data.frame(x = c(1979, 1982), y = 85), aes(x,y),
                size = 3, color=BLUE) + 
      # two highlight end points
      geom_point(data = data.frame(x = 2021, y = 67), aes(x,y),
                 fill = BROWN_DARKER, size = 7, pch = 21,
                 color = "white", stroke = 3
      ) + 
      geom_point(data = data.frame(x = 2021, y = 47.5), aes(x,y),
                 fill = BLUE, size = 7, pch = 21,
                 color = "white", stroke = 3
      ) 


p = p + 
      # horizontal tick text
      geom_text(
        data = data.frame(x = 2022, y = seq(10, 100, by = 10)), aes(x, y, label = y),
        hjust = 1, vjust = 0, nudge_y = 32 * 0.01,
        size = 6, alpha = 0.4
      ) + 
      # title
      geom_text(
        data = data.frame(x = 1979, y = 105, t = "County-Level Average Number of Extreme Heat Days (Annual, 1979-2021)"),
        aes(x, y, label = t), fontface = "bold",
        hjust = 0, vjust = 1, size = 7
      ) + 
      # legend text
      geom_text(
        data = data.frame(x = 1982.3, y = 89, t = "Georgia"),
        aes(x, y, label = t),
        hjust = 0, vjust = 0, size = 8
      ) + 
      geom_text(
        data = data.frame(x = 1982.3, y = 84, t = "ARC 11-County Region"),
        aes(x, y, label = t),
        hjust = 0, vjust = 0, size = 8
      ) +
      # stress point annotation 
      geom_text(
        data = data.frame(x = 2021.5, y = 67, t = "67"),
        aes(x, y, label = t), fontface = "bold",
        hjust = 1.5, vjust = -0.8, size = 15
      ) + 
      geom_text(
        data = data.frame(x = 2021.5, y = 47.5, t = "48"),
        aes(x, y, label = t), fontface = "bold",
        hjust = 1.5, vjust = -0.8, size = 15
      ) +
      # us average annotation text
      geom_text(data = data.frame(x = 2010, y = 18, t = 'US County Average (2021): 15'),
                aes(x,y, label = t),
                vjust = 0.4, size = 6, alpha = 0.4
                )

p = p + 
    theme(
      # remove background and border
      panel.border = element_blank(), 
      panel.background = element_blank(),
      # Remove vertical grid lines
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
      axis.title = element_blank(),
      # Only the bottom line of the vertical axis is painted in black
      axis.line.x.bottom = element_line(color = "black"),
      # But customize labels for the horizontal axis
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 15),
      plot.margin=unit(c(1,2,1,0), "cm"),
      panel.margin=unit(c(1,2,1,0), "cm")
    ) + 
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
    ) 

ggsave('output/line1.jpeg', plot = p, width = 16, height = 9)
