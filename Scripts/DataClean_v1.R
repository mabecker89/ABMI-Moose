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

dens_df <- metrics %>%
  filter(Response == "Density",
         Species == "Moose",
         !is.na(WMU),
         !WMU == "506;510" & !WMU == "530 North" & !WMU == "530 South") %>%
  mutate(WMU = as.numeric(str_replace_all(WMU, "530 Full", "530"))) %>%
  select(WMU, StartYear, MetricNum) %>%
  rename(Survey_Year = StartYear,
         Density = MetricNum) %>%
  full_join(gov_dens, by = c("WMU", "Survey_Year")) %>%
  # WMUs 258, 511, and 519 have different density estimates. 
  mutate(Density = if_else(is.na(Density.y), Density.x, Density.y)) %>%
  select(-c(Density.x, Density.y)) %>%
  distinct() %>%
  mutate(WMU = as.character(WMU))

# Plot density over time

dens <- dens_df %>%
  filter(Density < 1) %>%
  ggplot(aes(x = Survey_Year, y = Density, colour = WMU)) +
  geom_line() +
  geom_point(size = 2.5)

dens1 <- ggplotly(dens)

dens1









