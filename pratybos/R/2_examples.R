library(tidyverse)

### Linear regression
data(mpg, package="ggplot2")

mpg_select <- mpg[mpg$manufacturer %in% c("audi", "ford", "honda", "hyundai"), ]

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(mpg_select, aes(displ, cty)) + 
  labs(subtitle="mpg: Displacement vs City Mileage",
       title="Bubble chart")
#  An internal-combustion engine's displacement is the combined volume of air moved — or displaced — by the pistons in its cylinders
g + geom_jitter(aes(col=manufacturer, size=hwy)) + 
  geom_smooth(aes(col=manufacturer), method="lm", se=F)

# Let's try it with our data 

data <- read_csv("../../laboratorinis/data/lab_sodra.csv")

library(lubridate)

data %>%
  filter(str_detect(municipality, "Viln|Kaun|Klaip")) %>%
  mutate(month = parse_date_time(month, "ym")) %>%
  group_by(municipality, month) %>%
  summarise(avgWage = mean(avgWage, na.rm = TRUE),
            numInsured = sum(numInsured)) %>%
  ggplot(aes(x = month, y = avgWage, color = municipality)) +
  geom_point(aes(size = numInsured)) +
  geom_smooth(method="glm", se=T) +
  ylab("Vidutinis atlyginimas") +
  ylim(0, 2000) +
  xlab("")

# Density plot

theme_set(theme_classic())
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
labs(title="Density plot", 
     subtitle="City Mileage Grouped by Number of cylinders",
     caption="Source: mpg",
     x="City Mileage",
     fill="# Cylinders")

# our data

data %>%
  filter(str_detect(municipality, "Viln|Kaun|Klaip"),
         avgWage < 5000) %>%
  ggplot(aes(avgWage, fill = municipality)) +
  geom_density(alpha=0.5)


# Interactive plot

library(plotly)

x <- c(1:100)
random_y <- rnorm(100, mean = 0)
data2 <- data.frame(x, random_y)

fig <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')
fig

#our data

data %>%
  filter(str_detect(municipality, "Viln|Kaun|Klaip")) %>%
  mutate(month = parse_date_time(month, "ym")) %>%
  group_by(municipality, month) %>%
  summarise(avgWage = mean(avgWage, na.rm = TRUE),
            numInsured = sum(numInsured)) %>%
  plot_ly(x = ~month, y = ~avgWage, color = ~municipality, type = 'scatter', mode = 'lines')
