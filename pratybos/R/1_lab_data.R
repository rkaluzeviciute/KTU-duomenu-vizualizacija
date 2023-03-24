library(tidyverse)
library(jsonlite)

# JSON
download.file("https://atvira.sodra.lt/imones/downloads/2023/monthly-2023.json.zip", "temp")
unzip("temp")
list.files(getwd())
df <- fromJSON("monthly-2023.json")
file.remove("temp")
file.remove("monthly-2023.json")

# Lab Data
data <- read_csv("../../laboratorinis/data/lab_sodra.csv")

summary(data)

data$municipality <- as.factor(data$municipality)


data %>%
  group_by(ecoActName) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

hist(data$avgWage, breaks = 200)

## 1 uzduotis

data %>%
  filter(tax < 1000000) %>%
  ggplot(aes(x = tax)) +
  geom_histogram(bins = 200)

## 2 uzduotis

top5 <- data %>%
  group_by(name, code) %>%
  summarise(wage = max(avgWage)) %>%
  arrange(desc(wage)) %>%
  head(5)

data %>%
  filter(code == 976923) %>%
  ggplot(aes(x = month, y = avgWage)) +
  geom_line() + 
  geom_point() +
  theme_bw()

ggsave("SB TURTO FONDAS.png", width = 10, height = 10)
