
## https://www.census.gov/geographies/mapping-files/2013/geo/carto-boundary-file.html
## "shp/cb_2013_us_state_20m.shp"
library(sf)
library(dplyr)

shp_file <- here::here("data-raw/cb_2013_us_state_20m/cb_2013_us_state_20m.shp")

states_shp <- sf::st_read(shp_file)

usethis::use_data(states_shp)

#usethis::use_data(states_shp, internal = TRUE, overwrite = TRUE)
