# Data wrangling with Tuskegee Airmen data
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-02-15

library(ggplot2)
library(dplyr)

# Download file from Tidy Tuesday website
download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv",
              destfile = "airmen.csv")

# Load data into R's memory
airmen <- read.csv(file = "airmen.csv")

# Look at first six rows of data
head(airmen)

# Get summary stats for each column of data
summary(airmen)

# Look at distribution of values in graduated_from column
table(airmen$graduated_from)

# Rename column for easier ggplotting
airmen <- airmen %>%
  rename(num_credits = number_of_aerial_victory_credits)

# Boxplot of aerial victory credits per graduated from
ggplot(data = airmen, mapping = aes(x = graduated_from, 
                                    y = num_credits)) +
  geom_boxplot() + 
  geom_jitter()

# Look at distribution of values in pilot_type column
table(airmen$pilot_type)

# Update data to correct for misspelling "liaison"
airmen <- airmen %>%
  mutate(pilot_type = gsub(pattern = "Liason pilot",
                           replacement = "Liaison pilot",
                           x = pilot_type))

# Did replacement work (i.e. "Liason" is gone)
airmen %>% distinct(pilot_type)

# Rename column for easier wrangling
airmen <- airmen %>%
  rename(grad_rank = rank_at_graduation)

# Update rank_at_graduation for duplicate categories
airmen <- airmen %>%
  mutate(grad_rank = case_when(grad_rank == "Capt" ~ "Captain",
                               grad_rank == "N/A" ~ NA_character_,
                               grad_rank == "Unk" ~ NA_character_,
                               TRUE ~ grad_rank))

ggplot(data = airmen, mapping = aes(x = pilot_type,
                                    y = num_credits)) +
  geom_boxplot() + 
  geom_jitter()

# Quick check on number of airmen per state
airmen %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Subset data to those who have at least one credit
airmen_credited <- airmen %>%
  filter(num_credits > 0)

# Plot number of credits per state (only those with > 2 credits)
# On the X axis we have # credits and on the Y the count
ggplot(data = airmen_credited %>% filter(num_credits > 2), 
       mapping = aes(x = num_credits,
                     fill = state)) +
  geom_bar()

# Plot number of credits by graduation rank
ggplot(data = airmen, mapping = aes(x = grad_rank, y = num_credits)) +
  geom_boxplot() + 
  geom_jitter()

# Density plot of graduation rank by pilot type
# Start by getting counts of each combination of graduation rank and 
# pilot type
airmen_summary <- airmen %>%
  group_by(grad_rank, pilot_type) %>%
  summarize(count = n())

ggplot(data = airmen_summary, mapping = aes(x = grad_rank,
                                    y = pilot_type,
                                    fill = count)) +
  geom_tile()

# Question: What is the %% in the case_when documentation?
# The modulo (remainder) operator
10 %% 5
# 0
10 %% 4
# 2
# Modulo can be useful for reporting
for (i in 1:100) {
  # Only print if i is evenly divisible by 10
  if (i %% 10 == 0) {
    cat("i = ", i, "\n")
  }
}
