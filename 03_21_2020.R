library(tidyverse)
library(skimr)
library(lubridate)
library(gghighlight)
library(ggthemes)
library(ggThemeAssist)

## Download Data from https://covid.ourworldindata.org/data/ecdc/full_data.csv
## I manually cleaned and entered data for Turkey, the dataset used for this script can be found in the repo
read_csv(file = "full_data.csv") -> full_data


# Cleaning Data
clean_data <- full_data %>%
  filter(total_cases != 0) %>%  #some rows have all zeros
  mutate(date = mdy(date),
         location = as_factor(location)) %>%
  group_by(location) %>%
  mutate(first_event = date[1], #calculating days since first confirmed case
         days_since = as.double(date - first_event)) %>%
  select(-first_event)


## Plotting a single country first
turkey_plot <- ggplot(data = clean_data %>%
                     filter(location == "China"|
                            location == "Czech Republic"|
                            location == "Denmark"|
                            location == "France"|
                            location == "Germany"|
                            location == "Greece"|
                            location == "Iran"| 
                            location == "Italy"|
                            location == "Japan"| 
                            location == "Luxembourg"|
                            location == "South Korea"| 
                            location == "Spain"|
                            location == "Singapore"|
                            location == "Turkey"|
                            location == "United States")) +
  geom_line(aes(x = days_since, 
                y = total_cases, 
                color = location), size = 1) +
  gghighlight(location == "Turkey", label_params = element_blank()) +
  geom_point(aes(x = days_since, 
                 y = total_cases, 
                 color = location), size = 1) +
  coord_cartesian(xlim = c(0, 50)) +
  scale_y_log10(breaks = c(10^1, 10^2, 10^3, 10^4, 10^5),
                labels = c("10", "100", "1000", "10000", "100000")) +
  labs(title = "Confirmed Covid-19 Cases in 15 Countries",
       subtitle = "Data by European Centre for Disease Prevention and Control",
       x = paste("Number of days since first confirmed case", "\U2192"),
       y = element_blank()) +
  theme_classic() +
  theme(legend.position = "none")

## Show Plot
turkey_plot


## Facetting all countries
spread_plot <- ggplot(data = clean_data %>%
                     filter(location == "China"|
                              location == "Czech Republic"|
                              location == "Denmark"|
                              location == "France"|
                              location == "Germany"|
                              location == "Greece"|
                              location == "Iran"| 
                              location == "Italy"|
                              location == "Japan"| 
                              location == "Luxembourg"|
                              location == "South Korea"| 
                              location == "Spain"|
                              location == "Singapore"|
                              location == "Turkey"|
                              location == "United States")) +
  geom_line(aes(x = days_since, 
                y = total_cases,
                group = location), size = 1, colour = "red") +
  gghighlight(vars(location),
              use_group_by = FALSE,
              use_direct_label = FALSE) +  #don't want to see names of countries on the graph
  geom_point(aes(x = days_since, y = total_cases, group = location),
             size = 0.9,
             colour = "red",
             alpha = 0.2) +
  coord_cartesian(xlim = c(0, 50)) +
  scale_y_log10(breaks = c(10^1, 10^2, 10^3, 10^4, 10^5),
                labels = c("10", "100", "1000", "10000", "100000")) +
  labs(x = "Number of days since first confirmed case",
       y = element_blank(),
       title = "Total number of Covid-19 cases across different countries",
       subtitle = "Data by European Centre for Disease Prevention and Control") +
  theme(legend.position = "none") +
  facet_wrap(~ location, ncol = 5) +
  theme_wsj() +
  theme(axis.title = element_text(family = "sans", size = 13, color = "black", face = "bold", vjust = 0),
        strip.text = element_text(hjust = 0),
        plot.title = element_text(family = "sans", size = 18, color = "black", face = "bold", vjust = 0),
        plot.subtitle = element_text(family = "sans", size = 9, color = "black", face = "italic", vjust = 0))

## Show Plot
spread_plot
