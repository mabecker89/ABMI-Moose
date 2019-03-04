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
  group_by(Title) %>%
  # Count = number of 'observations' per source
  summarise(count = n()) %>% 
  arrange(desc(count))
  # OK, what else would we want to know about each source?





