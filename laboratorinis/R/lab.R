library(tidyverse)
library(jsonlite)

download.file("https://atvira.sodra.lt/imones/downloads/2022/monthly-2022.csv.zip", "temp")
unzip("temp")
raw <- read.csv("monthly-2022.csv", sep = ";")
names(raw) <- c("code", "jarCode", "name", "municipality", "ecoActCode", "ecoActName", "month", "avgWage", "numInsured", "avgWage2", "numInsured2", "tax")

codes <- raw %>%
  group_by(ecoActCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  head(22)

raw %>%
  filter(ecoActCode %in% codes$ecoActCode) %>%
  write_csv("lab_sodra.csv")

raw_json <- head(raw) %>%
  toJSON()

write(raw_json, "lab_sodra.json")

#1 uzduotis
filtered_data=
  lab_sodra%>%
  filter(grepl("68100", ecoActCode))

library(ggplot2)
ggplot(data = filtered_data, aes(x = avgWage)) +
  geom_histogram(color="red", fill="white")+
  labs(title = "Histogram of avgWage",
       x = "avgWage",
       y = "Count")

# 2 uzduotis
#data2 - kienvienos imones avgWage suskaiciuojam
data2=       
  filtered_data%>%
  group_by(name)%>%
  summarise(avg_Wage=mean(avgWage, na.rm=TRUE))%>%
  arrange(desc(avg_Wage))  

new_data = filtered_data %>% 
  left_join(data2, by = "name") #prijungiam prie pradinio

new_data=unique(new_data[, c("name", "avg_Wage")]) #paliekam nesikartojancius

new_data=new_data%>%   #randam 5 su didziausiu avgWage
  arrange(desc(avg_Wage))%>%
  head(5)

datamonths = 
  filtered_data%>%
  filter(name %in% new_data$name)

ggplot(datamonths, aes(x=month, y=avgWage, group=name, color=name))+
  geom_line()


#3 uzduotis
insured_filtered=
  datamonths%>%
  filter(month>=202204 & month<=202304)%>%
  group_by(name)%>%
  summarise(insured=sum(numInsured))

ggplot(insured_filtered, aes(x=reorder(name, -insured), y=insured))+
  geom_bar(stat="identity", color="black", fill="lightblue",
           linetype="dashed")+
  labs(x = "Name", y = "numInsured", title = "numInsured by Name")
