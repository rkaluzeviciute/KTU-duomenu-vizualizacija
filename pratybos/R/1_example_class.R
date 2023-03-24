library(tidyverse)
library(stringr)
# https://www.kaggle.com/prasertk/defense-contractors-market-cap-revenue-earning?select=defense+contractor.csv

data <- read_csv("../data/defense contractor.csv")

data %>%
  filter(Year == "2022") %>%
  group_by(Country) %>%
  mutate(market_cap = str_remove_all(`Market cap`, "B|\\$")) %>%
  # select(market_cap) %>%
  mutate(market_cap_num = as.numeric(trimws(market_cap))) %>%
  summarise(n = sum(market_cap_num)) %>%
  mutate(Country = fct_reorder(Country, n)) %>%
  ggplot(aes(x = Country, y = n)) +
  geom_col()


