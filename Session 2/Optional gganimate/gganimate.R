
library(tidyverse)
library(dplyr)
library(ggplot2)

library(gganimate)
library(readxl)
library(ggthemes)

library(gifski)

#MAC
setwd("/Users/andywilson1/Documents/GitHub/Jenny_Informatics_class/Session 2/Optional gganimate") 
#PC
#setwd("C:/Users/wilso/Box/gganimate")


mydata0 <-read.csv(url("https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/us-governors/data/us-governors.csv"), na.strings = c("", "NA")) %>%
  dplyr::select(state_code, state_name, party) %>%
  rename(state = state_code)
mydata2 <-read.csv(url("https://covidtracking.com/api/v1/states/daily.csv"), na.strings = c("", "NA"))

mydf <- merge(mydata0, mydata2 , by="state")
mydf$Date <- as.Date(as.character(mydf$date), "%Y %m %d")

mydf2 <- mydf %>%
  filter(Date >= "2020-06-01")



#For population size
pop <- read.csv(file = "COVID-19 US state policy database.csv", na.strings = c("", "NA")) %>%
  rename(state_name = Province_State) %>%
  select(state_name, Population.2018)


mydf3 <- merge(pop, mydf2 , by="state_name")

# politics <- read_excel("PoliticalLeaningsUSStates.xlsx") %>%
#   

mydf3$cum_incidence_100k <- mydf3$positive/mydf3$Population.2018*100000
mydf3$testing_100k <- mydf3$totalTestResults/mydf3$Population.2018*100000




theme_set(theme_few(base_size = 16, base_family = "serif"))
#library(ggrepel)
p <- ggplot(
  mydf3, 
  aes(x = testing_100k, y = cum_incidence_100k, size = Population.2018, label = state, colour = party)
) +
  geom_point(show.legend = FALSE, alpha = 0.5) +
  #scale_color_viridis_d() +
  scale_size(range = c(2, 20)) +
  scale_x_log10() + 
  geom_text( show.legend = FALSE, aes(colour = party)) + #check_overlap = TRUE,
  #geom_label( show.legend = FALSE, overlap_check = TRUE)+
  labs(x = "Testing Rate (per 100,000)", y = "Confirmed cases (per 100,000)", 
       colour = "Political leaning (2016)", size = "Population size") +
  scale_color_manual(values=c("blue", "red"))
p

# z <- p + transition_time(date) +
#   labs(title = "date: {frame_time}")


Z <- p + facet_wrap(~party) +
  transition_time(Date) +
  labs(title = "date: {frame_time}") #+
Z
  #shadow_wake(wake_length = 0.1, alpha = FALSE)

 # ZZ <- p + facet_wrap(~party) +
 #   transition_time(date) +
 #   labs(title = "date: {frame_time}") +
 #   shadow_wake(wake_length = 0.1, alpha = FALSE)
#Z

final_animation<-animate(Z, 100, fps = 30, duration = 30, width = 650, height = 650, renderer = gifski_renderer())
anim_save("Confirmed cases blue red.gif",animation=final_animation)

