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
st_write(fips, "Data\\counties_simple.shp", delete_dsn = T)
