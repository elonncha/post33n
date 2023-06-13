library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)

#' -----------------------------------------------------------------------------
#' @data
suppressMessages({
  county.atlregion = read_csv('data/raw/Counties_Atlanta_Region.csv')
  COUNTYFP.ARC = county.atlregion$GEOID10[county.atlregion$Reg_Comm == 'Atlanta Regional Commission']
  
  rm(county.atlregion)
  
  historic.heat = read_csv('data/cleaned/historic-heat-ga.csv')
  
  historic.heat.ARC = historic.heat %>% 
    filter(COUNTYFP %in% COUNTYFP.ARC, year<=2019)
  
  historic.heat.ARCavg = historic.heat %>% 
    filter(COUNTYFP %in% COUNTYFP.ARC) %>%
    group_by(year) %>% 
    summarise(avg_heat_days = mean(heat_days))
})
#' -----------------------------------------------------------------------------




#' -----------------------------------------------------------------------------
#' @format
font = "Gudea"
font_add_google(family=font, font, db_cache = TRUE)
fa_path = systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
theme_set(theme_minimal(base_family = font, base_size = 10))
bg = "#F4F5F1"
txt_col = "black"
showtext_auto(enable = TRUE)
#' -----------------------------------------------------------------------------


p = ggplot() + 
      geom_line(data = historic.heat.ARC, 
                aes(x = year, y = heat_days, color = COUNTYNAME), size = 0.5) + 
      gghighlight(use_direct_label = FALSE,
                  unhighlighted_params = list(colour = alpha("grey85", 1))) +
      geom_line(data = historic.heat.ARC, 
                aes(x = year, y = heat_days, color = COUNTYNAME), size = 1.5) + 
      geom_point(data = historic.heat.ARC %>%group_by(COUNTYNAME)%>%slice_max(year),
                 aes(x = year, y = heat_days, color = COUNTYNAME), 
                 shape = 16, size = 3.5) +
      geom_text(data=historic.heat.ARC %>% 
                  group_by(COUNTYNAME) %>% 
                  slice_max(year),
                aes(x=year, y=heat_days, color=COUNTYNAME, label = round(heat_days)),
                hjust = -.6, vjust = .7, size=15, family=font, fontface="bold") + 
      geom_smooth(data = historic.heat.ARC, aes(x = year, y = heat_days), 
                  method = 'loess', span = 30, size = 0.5, se=F,
                  color = "black", alpha = 0.3, linetype = "longdash") + 
      scale_color_met_d(name="Redon") +
      facet_wrap(~ COUNTYNAME)

p = p + 
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=25),
    strip.text.x = element_text(face="bold", size=55, hjust=0.5, vjust = 0.8),
    panel.background = element_blank(),
    plot.margin = margin(10,50,10,10),
    legend.position = "none"
  ) + 
  scale_x_continuous(
    limits = c(1978, 2022),
    expand = c(0, 0),
    breaks = seq(1980, 2020, 10),  
    labels = c("1980", "90", "00", "10", "20")
  )


ggsave('output/line2.jpeg', plot = p, width = 16, height = 9)

