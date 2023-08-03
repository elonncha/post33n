library(tidycensus)
library(tidyverse)
library(tmap)
library(ggplot2)

census_api_key('2fceb417afc726b59682b4b02cddb22f67445c55', install= T,
               overwrite = T)
var21 =  load_variables(2021, "acs5", cache = TRUE) %>% filter(geography == 'tract')


df = get_acs(geography = 'tract',
        state = 'GA',
        variables = c("DP04_0123PE", "DP04_0124PE", "DP04_0141PE", "DP04_0142PE"), 
        year = 2021
        ) %>%
        spread(key = variable, value = estimate) %>% 
        mutate_all(~replace(., is.na(.), 0))


shp = get_acs(geography = 'tract',
                   state = 'GA',
                   variables = c("B01001_001"),
                   year = 2021,
                   geometry = T) %>% select(GEOID,NAME) %>%
  separate(NAME, into = c('c','county','st'), sep = ',') %>%
  select(-c, -st) %>%
  filter(county %in% c(' Cherokee County', ' Clayton County', ' Cobb County', 
                       ' DeKalb County', ' Douglas County', ' Fayette County', 
                       ' Forsyth County', ' Fulton County', ' Gwinnett County', 
                       ' Henry County', ' Rockdale County'))


burden = df %>% group_by(GEOID) %>% 
  summarise(SMOCAPI = sum(DP04_0124P),
            GRAPI = sum(DP04_0142P)) 


tm_shape(shp %>% left_join(burden, by = 'GEOID')) + 
  tm_polygons(col = 'GRAPI',
              style = 'pretty',
              border.col = 'white',
              title = 'GRAPI>35%'
              #breaks = c(-Inf, 40, Inf)
              )


ggplot(burden %>% filter(GRAPI > 0, GRAPI < 100), aes(x = GRAPI)) + 
    geom_histogram(color="black", fill="white",binwidth=2)


              