library(tidyverse)
library(dygraphs)
library(xts)

mydata <- read.csv(url('https://covid.ourworldindata.org/data/owid-covid-data.csv'), na.strings = c("", "NA"))

country <- mydata %>%
  filter(location == "Iceland")


cases.ts <- xts(country$people_fully_vaccinated_per_hundred , order.by=as.POSIXct(country$date ))

dygraph(cases.ts, main = paste("Rolling COVID people fully vaccinated (per hundred) ", country$location[1])) %>%
  dyRoller(rollPeriod = 20) %>%
  dySeries("V1", strokeWidth = 3, label = "5-day rolling vaccinated") %>%
  dyShading(from = "2021-01-01", to = Sys.Date(), color = "#FFE6E6") %>%
  dyRangeSelector()





setwd("/Users/andywilson1/Documents/GitHub/Jenny_Informatics_class/Session 2.5")
data <- read.csv(file = "all-states-history.csv", na.strings = c("", "NA"))


state <- data %>%
  filter(state == "NY") %>%
  filter(date > "2020-06-01")

cases.ts <- xts(state$hospitalizedCurrently , order.by=as.POSIXct(state$date ))

dygraph(cases.ts, main = paste("Rolling COVID hospitalized Currently ", state$state[1])) %>%
  dyRoller(rollPeriod = 5) %>%
  dySeries("V1", strokeWidth = 3, label = "5-day rolling hospitalized (currently)") %>%
  dyShading(from = "2021-01-01", to = Sys.Date(), color = "#FFE6E6") %>%
  dyRangeSelector()



# Big data 
# https://data.cdc.gov/browse
big <- read.csv(file = "Provisional_COVID-19_Deaths_by_Sex_and_Age.csv", na.strings = c("", "NA"))
bigstate <- big %>%
  filter(State == "New York") %>%
  filter(Age.Group == "All Ages") %>%
  filter(Group == "By Month") %>%
  filter(Year == 2021)


ggplot(bigstate, aes(x=Month)) + 
  geom_line(aes(y=COVID.19.Deaths, col=Sex, lwd=Total.Deaths)) +
  ggthemes::theme_few()


