
# Load packages -----------------------------------------------------------
#general data wrangling
library(tidyverse)
#for exploring data
library(skimr)
#for getting and plotting map data
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2020-08-18')

plants <- tuesdata$plants
actions <- tuesdata$actions
threats <- tuesdata$threats

skim(plants)
skim(actions)
actions$action_taken
skim(threats)
threats$threat_type |> unique()

ggplot(threats, aes(threat_type)) +
  geom_bar()

threats |> 
  filter(threatened == 1) |> 
  ggplot(aes(group)) +
  geom_bar() +
  facet_wrap(~threat_type) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

actions |> 
  filter(action_taken == 1, action_type != "Unknown") |> 
  count(action_type)

n_threat_country <-
  threats |> 
  filter(threat_type == "Agriculture & Aquaculture") |> 
  group_by(country) |> 
  summarize(n_sp = sum(threatened))


# Get a world map --------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot() +
  geom_sf(data = world)


# Join data ---------------------------------------------------------------
n_threat_country <-
  n_threat_country |> 
  mutate(country = case_when(
    country == "Viet Nam" ~ "Vietnam",
    .default = country
  ))

# full join because we want to keep all the countries.  `world` on the left because we want the result to be an `sf` object for nice easy plotting
plot_df <- full_join(world, n_threat_country, by = join_by(name == country))

class(plot_df)

# Make a map --------------------------------------------------------------

ggplot(plot_df) +
  geom_sf(aes(fill = n_sp)) +
  scale_fill_viridis_c(na.value = "antiquewhite", end = .75, n.breaks = 10) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#c7ecfc"),
    panel.grid.major = element_line(color = "grey50"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(
    fill = guide_colorsteps(title = "", barwidth = grid::unit(10, "cm"),
                          barheight = grid::unit(0.25, "cm"))
  ) +
  labs(
    title = "Number of Plant Species Threatened by Agriculture & Aquaculture"
  ) 

