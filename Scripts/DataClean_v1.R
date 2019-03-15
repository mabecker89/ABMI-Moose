#-------------------------------------------------------------------------------

# Here is where we clean up the moose metrics data into a tidy dataframe.

# Load packages
library(tidyverse)

# Import data
metrics <- read_csv("./Data/Raw/Metrics.csv")

# First things first - let's pair it down to only moose.
metrics_m <- metrics %>%
  filter(Species == "Moose")

# Start with the data sources
df <- metrics_m %>%
  group_by(Title, Method) %>%
  # Count = number of 'observations' per source
  summarise(count = n()) %>% 
  arrange(desc(count))
  # OK, what else would we want to know about each source?

# Let's look at methods
df_meth <- metrics_m %>%
  group_by(Method) %>%
  count()
# Mostly census flights. Next most is telemetry - what are those studies telling
# us?
df_tel <- metrics_m %>%
  filter(WMU == "530 Full") %>%
  View()

         Response == "Density") %>%
  View()

df_year <- metrics_m %>%
  filter(Method == "CensusFlights",
         WMU == "515")

twentyeighteen <- metrics_m %>%
  filter(Method == "CensusFlights",
         StartYear == "2018")

#-------------------------------------------------------------------------------

library(mapproj)
library(sf)
library(tidyverse)


# Add OSM Moose sightings data

osm_moose <- read_csv("./Data/Raw/OSM_Ungulate_Survey_Data_2013-2018.csv")

osm_moose <- osm_moose %>%
  # Change NAs to WMU 515
  mutate(WMU = if_else(is.na(WMU), 515, WMU)) %>%
  filter(SPECIES == "MOOSE -MOOS") %>%
  select(-DATE_1) %>%
  mutate(WMU = as.character(WMU))

osm_moose %>%
  ggplot(aes(x = LONG, y = LAT, color = WMU)) + 
  geom_point() +
  coord_map() +
  theme_void()

ab_wmu %>%
  ggplot() +
  geom_sf()

osm_moose_sf <- osm_moose %>%
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326, agr = "constant") %>%
  expandRows("COUNT") %>%
  st_jitter(0.01)

names(st_geometry(ab_wmu)) = NULL

sightings <-
ab_wmu %>%
  separate(WMUNIT_COD, into = c("zeroes", "WMU"), sep = 2, remove = FALSE) %>%
  select(-zeroes) %>%
  filter(WMU %in% osm_moose_sf$WMU) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  
  addPolygons(color = "#282828", weight = 2, smoothFactor = 0.2, opacity = 2,
              group = "None") %>%
  addHeatmap(data = osm_moose_sf, radius = 10, blur = 18, minOpacity = 3)
              
sightings













