library(dplyr)
library(ggplot2)
library(wbstats)
library(tidyr)
library(knitr)

# General setup and Alex's
crime_rate_df <- read.csv("crime_rate_US_by_county.csv", stringsAsFactors = FALSE)
crime_rate_df$state <- substring(crime_rate_df$county_name, nchar(crime_rate_df$county_name) - 1, nchar(crime_rate_df$county_name))

crime_rate_WA_df <- crime_rate_df %>%
  filter(state == "WA")
crime_rate_WA_df$crime_rate_per_1000000 <- crime_rate_WA_df$crime_rate_per_100000 / 10

# plot of crime_rate_WA_df
crime_rate_plot <- ggplot(crime_rate_WA_df) +
  geom_col(mapping = aes(county_name, crime_rate_per_100000)) +
  labs(title = "Crime Rate / 100k in Each County of WA 2016", x = "County", y = "Crime Rate / 100k") +
  theme(axis.text.x = element_text(size = 5.5, angle = 90))

# plot of population
population_plot <- ggplot(crime_rate_WA_df) +
  geom_col(mapping = aes(county_name, population)) +
  labs(title = "Population in Each County of WA 2016", x = "County", y = "Population") +
  theme(axis.text.x = element_text(size = 5.5, angle = 90))

unemployment_df <- read.csv("unemployment.csv", stringsAsFactors = FALSE) %>%
  select(X.1, X.7) %>%
  rename("County" = X.1 , "Unemploy_rate"= X.7) %>%
  tail(-2) %>%
  head(-2)

unemployment_plot <- ggplot(unemployment_df) +
  geom_point(mapping = aes(County, Unemploy_rate)) +
  labs(title = "Unemployment Rate in Each County of WA 2016", x = "County", y = "Unemployment Rate") +
  theme(axis.text.x = element_text(size = 5.5, angle = 90))

crime_rate_WA_df$county_name <- substr(crime_rate_WA_df$county_name, 1, nchar(crime_rate_WA_df$county_name) - 4)
unemployment_df$County <- substr(unemployment_df$County, 1, nchar(unemployment_df$County) - 4)

crime_rate_WA_df <- rename(crime_rate_WA_df, County = county_name)
crime_unemp_df <- crime_rate_WA_df %>%
  left_join(unemployment_df, by = "County") %>%
  select(County, crime_rate_per_1000000, Unemploy_rate) %>%
  gather(key = Category, value = value, -County)

crime_unemp_wide <- crime_unemp_df %>%
  spread(key = Category, value = value)

crime_unemp_plot <- ggplot(data = crime_unemp_df, mapping = aes(x = County, y = value, color = Category)) +
  geom_point() +
  geom_path(mapping = aes(group = County, color = "#ccebc5"), show.legend = FALSE) +
  scale_color_discrete(breaks = c("crime_rate_per_1000000","Unemploy_rate"),
                       labels = c("Crime Rate /1000k", "Unemployment Rate (percent)")) +
  labs(title = "Crime Rate vs. Unemployment Rate by County in WA", y = "Value", color = "Category") +
  theme(axis.text.x = element_text(size = 5.5, angle = 90), legend.position = c(.19, .885))

# Dhandeep's part- Does the unemployment rate change as robbery/burglary increases?
top_five_pop <- crime_rate_WA_df %>%
  mutate(burg_robb_combined = BURGLRY+ROBBERY) %>%
  arrange(-population) %>%
  head(10) %>%
  select(1,2,22,27)

unemp_top_pop <- left_join(top_five_pop, unemployment_df, by = "County") %>%
  rename("unemployment_rate" = Unemploy_rate)

plot_unempl <- ggplot(data = unemp_top_pop , mapping = aes(x= unemployment_rate, y = burg_robb_combined, fill = County )) +
  geom_col() +
  labs(title = "10 Highest Robbery+Burglry attempts vs  Unemployment Rate", x = "Unemployemnt Rate", y = "Burglry + Robbery attempts")

crime_rate_WA_df <-
crime_rate_WA_df %>%
  select(c(
    County, crime_rate_per_100000,
    MURDER, RAPE, ROBBERY, AGASSLT,
    BURGLRY, LARCENY, MVTHEFT, ARSON, population
  ))


# Evelyn's
county_by_crime_rate <-
  crime_rate_WA_df %>%
  group_by(County) %>%
  arrange(crime_rate_per_100000) %>%
  select(
    County, crime_rate_per_100000
  )

county_murder_rate <-
  crime_rate_WA_df %>%
  group_by(County) %>%
      summarise(
        murder_rate = (MURDER/population)
      )

crime_rate_plot_E <-
  ggplot(
  data = county_by_crime_rate, mapping = aes(x = reorder(County, crime_rate_per_100000), y = crime_rate_per_100000,
                                             fill = "red")) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  labs(title = "Crime Rate per Counties in WA", x = "County", y = "Crime Rate Per 100,000") +
   theme(legend.position = "none")

murder_rate_plot <- ggplot(
  data = county_murder_rate, mapping = aes(x = reorder(County, murder_rate), y = murder_rate,
                                             fill = "red")) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  labs(title = "Murder Rate per Counties in WA", x = "County", y = "Murder Rate") +
  theme(legend.position = "none")

# Xinjie Huang
#rm(list = ls())
crime_rate_WA_df <- crime_rate_df %>% filter(state == "WA")
crime_rate_WA_df$crime_rate_per_1000000 <- crime_rate_WA_df$crime_rate_per_100000 / 10

crime_vs_type <- mutate(crime_rate_WA_df, Murder = MURDER / population, Rape = RAPE / population, Robbery = ROBBERY / population, "Agg Assault" = AGASSLT / population, Burglary = BURGLRY / population, Larceny = LARCENY / population, "MV Theft" = MVTHEFT / population, Arson = ARSON / population) %>% select(crime_rate_per_1000000, Murder, Rape, Robbery, "Agg Assault", Burglary, Larceny, "MV Theft", Arson) %>% gather(type, value, -crime_rate_per_1000000)
crime_vs_type_plot <- ggplot(data = crime_vs_type) +
  geom_point(mapping = aes(y = value, x = crime_rate_per_1000000)) +
  geom_line(mapping = aes(y = value, x = crime_rate_per_1000000, color = type)) +
  scale_color_brewer(palette = "RdYlGn", name = "Crime Type") +
  labs(title = "Crime Rate vs. Crime Type Per Capita", x = "Crime Rate (crime per 1 million population)", y = "Crime Type Per Capita") 

sample_crime_vs_type_table <- head(crime_vs_type, 10)

only_minmax_change <- group_by(crime_vs_type, type) %>% filter(crime_rate_per_1000000 == max(crime_rate_per_1000000) | crime_rate_per_1000000 == min(crime_rate_per_1000000)) %>% summarize(min = min(value), max = max(value)) %>% mutate(change = max - min)
plot_table <- select(only_minmax_change, type, change)
change_crime_type_plot <- ggplot(data = plot_table) +
  geom_bar(mapping = aes(x = reorder(type, change), y = change, color = type), stat = "identity") +
  labs(title = "Change in Crimes Per Capita", x = "Crime Type", y = "Change in Crime Per Capita between Min and Max Crime Rates") +
  scale_color_brewer(palette = "Dark2", name = "Crime Type")
