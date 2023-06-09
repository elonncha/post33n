library(tidyverse)
library(ggplot2)
library(sf)


#' -----------------------------------------------------------------------------
#' process raw heat data 

#' 0. @historic.heat
# VARIABLE(s): Number of Extreme Heat Days
# SOURCE: CDC National Environmental Public Health Tracking
historic.heat = read_csv('data/raw/historic-heat_ga_county.csv') %>% 
                select(StateFIPS, CountyFIPS, County, Year, Value) %>%
                rename(STATEFP = StateFIPS, COUNTYFP = CountyFIPS, COUNTYNAME = County, 
                       year = Year, heat_days = Value) %>%
                relocate(heat_days, .after = year)

write_csv(historic.heat, file = 'data/cleaned/historic-heat-ga.csv')


#' 1. @project.heat: 
# VARIABLE(s): Projected Difference in Extreme Heat Days as Compared to the Historical Period
# SOURCE: CDC National Environmental Public Health Tracking
project.heat = read_csv('data/raw/project-heat_ga_county.csv') %>%
               mutate(scenario = case_when(grepl("Low",`Emissions Scenario`) ~ 'low',
                                           .default = 'high')
                      ) %>%
               select(-State, -`Data Comment`, -`Absolute Threshold`, -`Start Year`, -`Emissions Scenario`) %>%
               rename(STATEFP = StateFIPS, COUNTYFP = CountyFIPS, COUNTYNAME = County, 
                      period = `End Year`, extra_heat_days = Value) %>%
               relocate(extra_heat_days, .after = scenario)

write_csv(project.heat, file = 'data/cleaned/project-heat-ga.csv')
#' -----------------------------------------------------------------------------              




#' -----------------------------------------------------------------------------
#' download and process county shapefile data 
#' Note: Us county file is not on GitHub folder as it exceeds 100M.
if (!("tl_2020_us_county.zip" %in% list.files(path = "./data/raw"))) {
  download.file(url = 'https://www2.census.gov/geo/tiger/TIGER2020/COUNTY/tl_2020_us_county.zip',
                destfile = 'data/raw/tl_2020_us_county.zip')
}
unzip('data/raw/tl_2020_us_county.zip', exdir = 'data/raw/tl_2020_us_county')

county.ga = st_read('data/raw/tl_2020_us_county/tl_2020_us_county.shp') %>% 
            filter(STATEFP == '13') %>%
            select(STATEFP, GEOID, NAME, geometry) %>%
            rename(COUNTYFP = GEOID, COUNTYNAME = NAME)
st_write(county.ga, 'data/cleaned/tl_2020_ga_county/tl_2020_ga_county.shp')
#' -----------------------------------------------------------------------------

