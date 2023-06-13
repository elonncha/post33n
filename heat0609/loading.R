library(sf)


suppressMessages({
  
  county.atlregion = read_csv('data/raw/Counties_Atlanta_Region.csv')
  historic.heat = read_csv('data/raw/historic-heat_ga_county.csv')
  
  COUNTYFP.ATLM = county.atlregion$GEOID10
  COUNTYFP.ARC = county.atlregion$GEOID10[county.atlregion$Reg_Comm == 'Atlanta Regional Commission']
  COUNTYFP.GA = unique(historic.heat$CountyFIPS)
  
  rm(county.atlregion)
  rm(historic.heat)
  
  historic.heat = read_csv('data/cleaned/historic-heat-ga.csv')
  project.heat = read_csv('data/cleaned/project-heat-ga.csv')
  ga.county.shp = st_read('data/cleaned/tl_2020_ga_county.shp')
  
  
})

print('File Loading Succeeds!')