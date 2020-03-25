## Simplifies the county polygon layer

library(dplyr)
library(readr)
library(sf)
library(rmapshaper)

fips <- st_read('csse_covid_19_data\\tl_2016_us_county.shp')
fips <- rmapshaper::ms_simplify(input = as(fips, 'Spatial'), keep = 0.02) %>%
  st_as_sf()

## Truly annoying, non-states like Puerto Rico, Virgin Islands, Guam are left 
## out. Need to start with Puerto Rico
fips <- fips[c(substr(fips$STATEFP, 1, 2) <= 59), ]
fips$ALANDKMSQ <- fips$ALAND/1e6
fips$AWATERKMSQ <- fips$AWATER/1e6
fips <- fips %>% select(-ALAND, -AWATER, -LSAD, -CLASSFP, -MTFCC, -CSAFP,
                        -CBSAFP, -METDIVFP, -FUNCSTAT, -INTPTLAT, -INTPTLON)

population <- read_csv('csse_covid_19_data\\co-est2018-alldata.csv') %>% 
  select(SUMLEV, STATE, COUNTY, STNAME, CTYNAME, CENSUS2010POP, POPESTIMATE2018)
population$STATE <- as.character(population$STATE)
population$COUNTY <- as.character(population$COUNTY)
population$STATE <- ifelse(nchar(population$STATE) == 1,
                           paste0("0", population$STATE),
                           population$STATE)
population$COUNTY <- ifelse(nchar(population$COUNTY) < 3,
                            paste0("0", population$COUNTY),
                            population$COUNTY)
population$COUNTY <- ifelse(nchar(population$COUNTY) < 3,
                            paste0("0", population$COUNTY),
                            population$COUNTY)
population$FIPS <- paste0(population$STATE, population$COUNTY)

fips_pop <- left_join(fips, population, by = c("GEOID" = "FIPS"))

write_csv(dplyr::filter(population, SUMLEV == 40), "Data\\state_pop.csv")
write_csv(dplyr::filter(population, SUMLEV == 50), "Data\\county_pop.csv")
st_write(fips_pop, "Data\\counties_simple.shp", delete_dsn = T)