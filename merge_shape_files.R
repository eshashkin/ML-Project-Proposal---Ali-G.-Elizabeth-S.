library(tigris)
library(purrr)
library(dplyr)

options(tigris_use_cache = TRUE)

# Get vector of all state abbreviations
states <- unique(fips_codes$state)[1:51]  # Excludes territories

# Download all PUMAs and bind into one
all_pumas <- map_df(states, ~pumas(state = .x, cb = TRUE, year = 2020))

# Save locally (optional)
sf::st_write(all_pumas, "~/Downloads/pumas_2022_nationwide.shp")

library(sf)

# Replace with your actual file path if not in Downloads
shp_path <- "~/Downloads/pumas_2022_nationwide.shp"
pumas_sf <- st_read(shp_path)
names(pumas_sf)
head(pumas_sf)
