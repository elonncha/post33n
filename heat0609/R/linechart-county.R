extrafont::font_import()
library(ggplot2)
library(tidyverse)
library(plotly)
library(hrbrthemes)
library(lubridate)
source('R/loading.R')

library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)

#' -----------------------------------------------------------------------------
#' @data
historic.heat.ARC = historic.heat %>% 
  filter(COUNTYFP %in% COUNTYFP.ARC, year<=2019)

historic.heat.ARCavg = historic.heat %>% 
  filter(COUNTYFP %in% COUNTYFP.ARC) %>%
  group_by(year) %>% 
  summarise(avg_heat_days = mean(heat_days))
#' -----------------------------------------------------------------------------

#### MISC ####
font <- "Gudea"
font_add_google(family=font, font, db_cache = TRUE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
theme_set(theme_minimal(base_family = font, base_size = 10))
bg <- "#F4F5F1"
txt_col <- "black"
showtext_auto(enable = TRUE)

caption_text  <- str_glue("**Design:** Gilbert Fontana<br>","**Data:** OECD, 2022")




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
                hjust = -.5, vjust = .5, size=8, family=font, fontface="bold") + 
      geom_smooth(data = historic.heat.ARC, aes(x = year, y = heat_days), 
                  method = 'loess', span = 30, size = 0.5, se=F,
                  color = "black", alpha = 0.3, linetype = "longdash") + 
      scale_color_met_d(name="Redon") +
      facet_wrap(~ COUNTYNAME)

p = p + 
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    #axis.text = element_text(color=txt_col, size=15),
    strip.text.x = element_text(face="bold", size=30, hjust=0.1, vjust = 0.8),
    plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
    plot.subtitle = element_markdown(hjust=.5,size=30, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
    plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=20, color=txt_col, lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(10,10,10,10),
    legend.position = "none",
    legend.title = element_text(face="bold")
  )


p
