
#install.packages("esquisse")
library(tidyverse)


mydata <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"), na.strings = c("", "NA"))
State <- "California" 
CASES <- mydata %>% 
  filter(Province_State ==State) %>% 
  select(starts_with("X"))

Y<- colSums(CASES) 
n <- as.numeric(length(Y)) 

temp1 <- data.frame(y = Y, x = seq(1:n)) 
temp1$date<-row.names(temp1) 

temp <- temp1 %>% filter(y > 0) 




#Let's esquisse out the code below: 

#Or use tools Addins
# esquisse:::esquisser()


library(ggplot2)

ggplot(temp) +
 aes(x = x, y = y) +
 geom_point(size = 1.18, colour = "#0c4c8a") +
 geom_smooth(span = 0.75) +
 ggthemes::theme_wsj()
