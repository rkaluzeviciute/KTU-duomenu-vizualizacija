library(tidyverse)
# library(jsonlite)
library(lubridate)
library(plotly)

### I paskaita

# JSON
# download.file("https://atvira.sodra.lt/imones/downloads/2023/monthly-2023.json.zip", "temp")
# unzip("temp")
# list.files(getwd())
# df <- fromJSON("monthly-2023.json")
# file.remove("temp")
# file.remove("monthly-2023.json")

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

### II paskaita

p1 <- data %>%
  filter(code %in% top5$code) %>%
  mutate(month = parse_date_time(month, "ym")) %>%
  ggplot(aes(x = month, y = avgWage, color = name)) +
  geom_line(size = 0.5, linetype = 2, alpha=0.4) + 
  geom_point(color = "red") +
  theme_bw()

plot(p1)

ggplotly(p1)

# total wage by company
# tax/wage ratio

p2 <- data %>%
  group_by(code, name) %>%
  summarise(avg_wage = mean(avgWage), avg_insured = median(numInsured), total_tax = sum(tax)) %>%
  na.omit() %>%
  arrange(desc(avg_wage)) %>%
  head(50) %>%
  ggplot(aes(x = avg_wage, y = total_tax, size = avg_insured, color = name)) +
  geom_point() +
  theme(legend.position = "none")
plot(p2)
ggplotly(p2)


## Simple regression example

data_stage <- data %>%
  group_by(code, name) %>%
  summarise(avg_wage = mean(avgWage), avg_insured = median(numInsured), total_tax = sum(tax)) %>%
  na.omit() %>%
  arrange(desc(avg_wage)) %>%
  head(100) 

# using geom_smooth

ggplot(data_stage) +
  geom_point(aes(x = avg_wage, y = total_tax, size = avg_insured, color = name)) +
  geom_smooth(aes(x = avg_wage, y = total_tax), method="glm", se=T) +
  theme(legend.position = "none")

ggsave("regression.png")

# using glm and custom line

reg <- glm(total_tax ~ avg_wage, data=data_stage)
reg


ggplot(data_stage, aes(x = avg_wage, y = total_tax, size = avg_insured, color = name)) +
  geom_point() +
  geom_abline(slope=reg$coefficients[2],
              intercept=reg$coefficients[1],
              color="red") +
  theme(legend.position = "none")



