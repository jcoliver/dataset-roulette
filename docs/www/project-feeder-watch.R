# Data Roulette
# April 2023
# EKB, Feeder Watch

# PACKAGES and DATA ####

library(tidyverse)
library(skimr)

feeder <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv")

# EXPLORE DATA ####

head(feeder)
glimpse(feeder)

# use the skim() function from the `skimr` package
skim(feeder)

# Check Longitudes and Latitudes
feeder %>% 
  filter(latitude < 0)

feeder %>% 
  filter(longitude > 0)

# flip coordinates with the wrong signs
feeder <- feeder %>% 
  mutate(latitude = if_else(latitude < 0, latitude * -1, latitude),
         longitude = if_else(longitude > 0, longitude * -1, longitude))

# PLOT THE LOCATION DATA ####

# plot the US
states <- map_data("state")
ggplot() +
  geom_map(data = states, map = states,
           aes(map_id = region),
           color = "black", fill = "white") +
  geom_point(data = feeder, aes(x = longitude, y = latitude)) +
  theme_classic()

# plot the world!
world <- map_data("world")
ggplot() +
  geom_map(data = world, map = world,
           aes(map_id = region),
           color = "black", fill = "white") +
  geom_point(data = feeder, aes(x = longitude, y = latitude)) +
  theme_minimal()

# COMMON SPECIES ####

# commonly observed species 

common_species <- feeder %>% 
  group_by(species_code) %>% 
  count()

arrange(common_species, desc(n)) 

ggplot(common_species, aes(x = n)) +
  geom_density() +
  theme_classic()

ggplot(common_species, aes(x = n)) +
  geom_histogram() +
  theme_classic()

# abundance

abundance <- feeder %>% 
  group_by(species_code) %>% 
  summarise(abundance = sum(how_many)) %>% 
  arrange(desc(abundance))

ggplot(abundance, aes(abundance)) +
  geom_histogram() +
  theme_bw()
