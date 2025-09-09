library(tidyverse)
library(tidymodels)
library(vroom)
library(skimr)
library(patchwork)

bike <- vroom("train.csv")

glimpse(bike)

skimr::skim(bike)

DataExplorer::plot_intro(bike)

DataExplorer::plot_missing(bike)

DataExplorer::plot_correlation(bike)

DataExplorer::plot_histogram(bike)

DataExplorer::plot_bar(bike)

GGally::ggpairs(bike %>% select(temp, atemp, humidity, windspeed, count))


# Hourly rentals
p1 <- bike %>%
  group_by(hour = lubridate::hour(datetime)) %>%
  summarise(avg_count = mean(count)) %>%
  ggplot(aes(x = hour, y = avg_count)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Average Rentals by Hour of Day",
       x = "Hour of Day", y = "Avg Count")

# Seasonal rentals
p2 <- bike %>%
  group_by(season) %>%
  summarise(avg_count = mean(count)) %>%
  ggplot(aes(x = factor(season), y = avg_count, fill = factor(season))) +
  geom_col() +
  labs(title = "Average Rentals by Season",
       x = "Season", y = "Avg Count") +
  theme(legend.position = "none")

# Weather situation
p3 <- bike %>%
  group_by(weather) %>%
  summarise(avg_count = mean(count)) %>%
  ggplot(aes(x = factor(weather), y = avg_count, fill = factor(weather))) +
  geom_col() +
  labs(title = "Average Rentals by Weather",
       x = "Weather Condition", y = "Avg Count") +
  theme(legend.position = "none")

# Temperature vs count
p4 <- bike %>%
  ggplot(aes(x = temp, y = count)) +
  geom_point(alpha = 0.3, color = "darkorange") +
  geom_smooth(method = "loess", color = "black") +
  labs(title = "Temperature vs Rentals",
       x = "Temperature (°C)", y = "Count")

# panel
(p1 | p2) / (p3 | p4)
