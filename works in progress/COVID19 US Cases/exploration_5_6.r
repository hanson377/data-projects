library(tidyverse)
library(lubridate)


covid <- read.csv("C:/Users/hanso/OneDrive/Documents/GitHub/data-projects/works in progress/COVID19 US Cases/data_5_6.csv")

covid$week <- isoweek(covid$date)
covid$month <- month(covid$date)
covid <- covid %>% group_by(county,state) %>% arrange(date) %>% mutate(daily_add = cases-lag(cases), perc_growth = (cases/lag(cases))-1 )


daily <- covid %>% group_by(date) %>% summarise(cases = sum(cases), deaths = sum(deaths))
weekly <- covid %>% group_by(week) %>% summarise(cases = sum(cases), deaths = sum(deaths))

ggplot(daily, aes(x = as.Date(date), y = cases)) + geom_bar(stat = 'identity')
ggplot(weekly, aes(x = week, y = cases)) + geom_bar(stat = 'identity')


## now by state
daily <- covid %>% group_by(date,state) %>% summarise(cases = sum(cases), deaths = sum(deaths)) %>% group_by(date) %>% mutate(share = cases/sum(cases))
ggplot(daily, aes(x = as.Date(date), y = share, colour = state)) + geom_bar(stat = 'identity') + theme(legend.position = 'none')


## pull latest week of data and identify those with largest growth
latest <- max(covid$week)

sample <- covid %>% filter(week == latest & cases >= 100 & perc_growth >= .15 & is.na(fips) != TRUE) %>% select(county,state,fips)
sample <- unique(sample)

data <- covid %>% inner_join(sample, by = c('county','state','fips')) %>% arrange(county,state,fips,date) %>% filter(month >= 4)
ggplot(data, aes(x=as.Date(date), y = cases, colour = factor(fips))) + geom_line(stat='identity')
ggplot(data, aes(x=as.Date(date), y = perc_growth, colour = factor(fips))) + geom_line(stat='identity')
ggplot(data, aes(x=as.Date(date), y = daily_add, colour = factor(fips))) + geom_line(stat='identity')
