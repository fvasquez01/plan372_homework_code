# load required libraries
library(tidycensus)
library(tidyverse)
library(sf)
library(ggplot2)

# set working directory and load data
setwd("/Users/francevasquez/desktop/372final")

# load charlotte shapefile and filter for mecklenburg county
charlotte_shape <- st_read("tl_2024_37_tract.shp") %>%
  filter(COUNTYFP == "119") %>%
  st_transform(crs = 4326)  # ensure same crs as bus stops

# load bus stop data
bus_stops <- read_csv("Bus_Stops_With_Frequency_HLT.csv")

# load acs variables
v23 <- load_variables(2023, "acs5", cache = TRUE)

variables <- c(
  total_population = "B01003_001",
  median_age = "B01002_001",
  white_alone = "B02001_002",
  black_alone = "B02001_003",
  asian_alone = "B02001_005",
  hispanic_or_latino = "B03003_003",
  median_income = "B19013_001",
  no_vehicle = "B08201_002",
  total_households = "B08201_001"
)

# download acs data
charlotte_data <- get_acs(
  geography = "tract",
  variables = variables,
  year = 2023,
  state = "NC",
  county = "Mecklenburg",
  geometry = FALSE,
  survey = "acs5",
  output = "wide"
)

# convert bus stops to spatial format
bus_stops_sf <- bus_stops %>%
  mutate(
    Frequency_Midday = Frquency_Midday  # fix typo
  ) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# visualize all bus stops on map
ggplot() +
  geom_sf(data = charlotte_shape, fill = "gray", color = "white") +
  geom_sf(data = bus_stops_sf, size = 0.8, color = "darkblue", alpha = 0.8) +
  labs(
    title = "All Bus Stops in Charlotte"
  ) +
  theme_minimal()

# convert frequency variables to numeric and calculate daily frequency
# this was done in order to calculate the daily frequency of bus stops, these variables had to be converted into numeric
bus_stops_sf <- bus_stops_sf %>%
  mutate(
    daily_frequency = rowSums(across(c(Frequency_AM_Peak, Frequency_Midday, Frequency_PM_Peak, Frequency_Late_Night), ~as.numeric(.)), na.rm = TRUE)
  )

# visualize bus stops by daily frequency
ggplot() +
  geom_sf(data = charlotte_shape, fill = "darkgreen", color = "black") +
  geom_sf(data = bus_stops_sf, aes(color = daily_frequency), size = 1, alpha = 0.7) +
  scale_color_viridis_c(option = "cividis", name = "Daily Frequency") +
  labs(
    title = "Charlotte Bus Stops - Daily Frequency"
  ) +
  theme_minimal()

# aggregate bus frequencies by census tract 
bus_stops_joined <- st_join(bus_stops_sf, charlotte_shape, join = st_intersects)

tract_frequencies <- bus_stops_joined %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(
    daily_frequency = sum(daily_frequency, na.rm = TRUE)
  )

charlotte_full <- charlotte_shape %>%
  left_join(charlotte_data, by = "GEOID") %>%
  left_join(tract_frequencies, by = "GEOID")

# run regression model on demographics and daily frequency
charlotte_model_data <- charlotte_full %>%
  transmute(
    GEOID,
    daily_frequency,
    total_population = total_populationE,
    median_age = median_ageE,
    white_pct = white_aloneE / total_populationE,
    black_pct = black_aloneE / total_populationE,
    asian_pct = asian_aloneE / total_populationE,
    hispanic_pct = hispanic_or_latinoE / total_populationE,
    median_income = median_incomeE,
    no_vehicle_pct = no_vehicleE / total_householdsE
  ) %>%
  drop_na()

model <- lm(daily_frequency ~ total_population + median_age + white_pct + black_pct +
              asian_pct + hispanic_pct + median_income + no_vehicle_pct,
            data = charlotte_model_data)

summary(model)

# predict daily frequency and map predicted demand 
charlotte_model_data$predicted_daily_freq <- predict(model, newdata = charlotte_model_data)

charlotte_predictions_daily <- charlotte_shape %>%
  left_join(st_drop_geometry(charlotte_model_data) %>% select(GEOID, predicted_daily_freq), by = "GEOID")

ggplot() +
  geom_sf(data = charlotte_predictions_daily, aes(fill = predicted_daily_freq), color = "black") +
  scale_fill_viridis(name = "Predicted Daily Frequency", option = "plasma") +
  labs(
    title = "Predicted Daily Bus Stop Demand by Census Tract"
  ) +
  theme_minimal()

# map of % households without vehicles
charlotte_novehicle_map <- charlotte_shape %>%
  left_join(st_drop_geometry(charlotte_model_data) %>% select(GEOID, no_vehicle_pct), by = "GEOID")

ggplot() +
  geom_sf(data = charlotte_novehicle_map, aes(fill = no_vehicle_pct * 100), color = "black") +
  scale_fill_viridis(name = "% No Vehicle", option = "inferno") +
  labs(
    title = "% of Households Without a Vehicle by Tract"
  ) +
  theme_minimal()

# map of low-income tracts (median income < $35000)
charlotte_low_income_map <- charlotte_full %>%
  mutate(is_low_income = median_incomeE < 35000)

ggplot() +
  geom_sf(data = charlotte_low_income_map, aes(fill = is_low_income), color = "black") +
  scale_fill_manual(values = c("gray", "darkred"), labels = c("$35K or more", "Below $35K"), name = "Median Income") +
  labs(
    title = "Census Tracts with Median Income Below $35,000"
  ) +
  theme_minimal()

# table of top tract by predicted daily frequency
tract_stop_counts <- bus_stops_joined %>%
  st_drop_geometry() %>%
  count(GEOID, name = "num_bus_stops")

top_tract <- charlotte_model_data %>%
  arrange(desc(predicted_daily_freq)) %>%
  slice(1:1) %>%
  left_join(tract_stop_counts, by = "GEOID") %>%
  select(GEOID, predicted_daily_freq, num_bus_stops, total_population, median_income, no_vehicle_pct,
         white_pct, black_pct, asian_pct, hispanic_pct) %>%
  mutate(across(c(no_vehicle_pct, white_pct, black_pct, asian_pct, hispanic_pct), ~ . * 100))

print(top_tract)

# get coordinates for top tract by predicted daily frequency 
top1_geoids <- charlotte_predictions_daily %>%
  st_drop_geometry() %>%
  arrange(desc(predicted_daily_freq)) %>%
  slice(1:1) %>%
  pull(GEOID)

top1_coords <- charlotte_predictions_daily %>%
  filter(GEOID %in% top1_geoids) %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(GEOID, predicted_daily_freq, lon, lat)

print(top1_coords)

# final recommendation 
current_freq_top1 <- charlotte_full %>%
  filter(GEOID %in% top1_geoids) %>%
  st_drop_geometry() %>%
  select(GEOID, daily_frequency)

recommendation_table <- top1_coords %>%
  left_join(current_freq_top1, by = "GEOID") %>%
  mutate(
    freq_gap = round(predicted_daily_freq - daily_frequency),
    proposed_additional_freq = ifelse(freq_gap < 5, 5, freq_gap)  
  )

print(recommendation_table)
