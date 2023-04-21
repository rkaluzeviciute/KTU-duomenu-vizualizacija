library(tidyverse)
library(jsonlite)

download.file("https://atvira.sodra.lt/imones/downloads/2022/monthly-2022.csv.zip", "temp")
unzip("temp")
raw <- read.csv("monthly-2022.csv", sep = ";")
names(raw) <- c("code", "jarCode", "name", "municipality", "ecoActCode", "ecoActName", "month", "avgWage", "numInsured", "avgWage2", "numInsured2", "tax")

#1 uzduotis
raw=
  raw%>%
  filter(grepl("681000", ecoActCode))

library(ggplot2)
graph=ggplot(data = raw, aes(x = avgWage)) +
  geom_histogram(color="red", fill="white")+
  labs(title = "Histogram of avgWage",
       x = "avgWage",
       y = "Count")
summary(raw)

# 2 uzduotis
data=       
  raw%>%
  group_by(name)%>%
  summarise(avg_Wage=mean(avgWage, na.rm=TRUE))%>%
  arrange(desc(avg_Wage))  

new_data=merge(raw, data, by = "name")
new_data=filter(new_data, name %in% data$name)

new_data=unique(new_data[, c("name", "avg_Wage")]) #paliekam nesikartojancius

new_data=new_data%>%   #randam 5 su didziausiu avgWage
  arrange(desc(avg_Wage))%>%
  head(5)

datamonths = 
  raw%>%
  filter(name %in% new_data$name)

ggplot(datamonths, aes(x=month, y=avgWage, group=name, color=name))+
  geom_line()


#3 uzduotis
insured_filtered=
  datamonths%>%
  filter(month>=202201 & month<=202212)%>%
  group_by(name)%>%
  summarise(max=max(numInsured))


ggplot(insured_filtered, aes(x=reorder(name, -max), y=max))+
  geom_bar(stat="identity", color="black", fill="lightblue",
           linetype="dashed")+
  labs(x = "Name", y = "numInsured", title = "numInsured by Name")
