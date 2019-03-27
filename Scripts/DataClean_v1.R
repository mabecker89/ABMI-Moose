#-------------------------------------------------------------------------------

# Clean original Metrics data into tidy dataset. 

# Goal: One observation per row. 

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
# Mostly census flights. 
# Next most is telemetry - what are those studies telling us?
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

# Government aerial observation data

gov_df <- metrics_m %>%
  filter(Method == "CensusFlights" | is.na(Method))













