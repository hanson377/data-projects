---
title: "Exploring the BRFSS data"
output:
  html_document:
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

## Setup

### Load packages

```{r load-packages, message = FALSE}
library(ggplot2)
library(dplyr)
```

### Load data

Make sure your data and R Markdown files are in the same directory. When loaded
your data file will be called `brfss2013`. Delete this note when before you submit
your work.

```{r load-data}
load("/Users/joshuahanson/Desktop/coursera/brfss2013.RData")
```



* * *

## Part 1: Data


* * *

## Part 2: Research questions

**Research quesion 1: How does the month surveyed impact an individuals feeling of hopelessness and depression? ** ## how many hours per week work, annual income

dep_per_month <- brfss2013 %>%
 group_by(imonth) %>%
 summarise(
   median_hop = median(qlmentl2, na.rm=TRUE),
   mean_hop = mean(qlmentl2, na.rm=TRUE)
)
ggplot(data = ex_per_month,aes(x = imonth, y = median_hop))+geom_point()+geom_line()
ggplot(data = ex_per_month,aes(x = imonth, y =  mean_hop))+geom_point()+geom_line()



**Research quesion 2: Does the month surveyed impact exercise levels? ** ## minutes/times spent walkinig, running, jogging and swimming

ggplot(brfss2013, aes(exerhmm1)) +
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=1,
                   position="dodge",
                   colour="black",
                   na.rm=TRUE)


ggplot(brfss2013, aes(exerhmm1)) + stat_ecdf()
quantile(brfss2013$exerhmm1, c(.80,.85, .90, .95, .98, .99), na.rm=TRUE)
## we can see that extreme values are causing issues.
## we can also see that most of our values lie under the 500 value.
## lets subset

health_data <- subset(brfss2013, exerhmm1 <= 200)
ggplot(health_data, aes(exerhmm1)) + stat_ecdf()

ggplot(health_data, aes(exerhmm1, colour = sex)) + stat_ecdf()
ggplot(health_data, aes(exerhmm1, colour = income2)) + stat_ecdf()
ggplot(health_data, aes(exerhmm1, colour = imonth)) + stat_ecdf()


ggplot(health_data, aes(imonth, exerhmm1)) + geom_boxplot()


ex_per_month <- health_data %>%
 group_by(imonth) %>%
 summarise(
   median_ex = median(exerhmm1, na.rm=TRUE),
   mean_ex = mean(exerhmm1, na.rm=TRUE)
)
ggplot(data = ex_per_month,aes(x = imonth, y = median_ex))+geom_point()+geom_line()
ggplot(data = ex_per_month,aes(x = imonth, y =  mean_ex))+geom_point()+geom_line()

## seems to be a seasonal effect!
## what activities iincrease in popularity during the summer?
## does this vary by state, income level, gender?

ex_per_month_by_income <- brfss2013 %>%
 group_by(imonth, income2) %>%
 summarise(
   median_ex = median(exerhmm1, na.rm=TRUE),
   mean_ex = mean(exerhmm1, na.rm=TRUE)
)
ggplot(data = ex_per_month_by_income,aes(x = imonth, y = mean_ex, color=factor(income2)))+geom_line(aes(group=income2))

ex_per_month_by_state <- brfss2013 %>%
 group_by(imonth, X_state) %>%
 summarise(
   median_ex = median(exerhmm1, na.rm=TRUE),
   mean_ex = mean(exerhmm1, na.rm=TRUE)
)
ggplot(data = ex_per_month_by_state,aes(x = imonth, y = mean_ex, color=factor(X_state)))+geom_line(aes(group=X_state))

ex_per_month_by_sex <- brfss2013 %>%
 group_by(imonth, sex) %>%
 summarise(
   median_ex = median(exerhmm1, na.rm=TRUE),
   mean_ex = mean(exerhmm1, na.rm=TRUE)
)
ggplot(data = ex_per_month_by_sex,aes(x = imonth, y = mean_ex, color=factor(sex)))+geom_line(aes(group=sex))
ggplot(data = ex_per_month_by_sex,aes(x = imonth, y = median_ex, color=factor(sex)))+geom_line(aes(group=sex))

## okay, the months are a little messy. so lets group the months by season.
brfss2013$starwars %>%
  select(i_month:idate) %>%
  mutate(
    type = case_when(
      idate <  3202013 ~ "winter",
      idate <  6202013 ~ "spring",
      idate <  9222013 ~ "summer",
      idate <  12212013 ~ "fall",
      idate <  12312013 ~ "winter"
    )
  )

brfss2013$season <- 'blank'
  brfss2013$season %>%
        case_when(
          idate <  3202013 ~ "winter",
          idate <  6202013 ~ "spring",
          idate <  9222013 ~ "summer",
          idate <  12212013 ~ "fall",
          idate <  12312013 ~ "winter"
)

**Research quesion 3: what is the relationship between alcohol consumption/diet and days depressed in the past 30 days?** ## ho wmany times did you eat fruit? how many times did you eat beans or lentils? days in past 30 with alcoholic beverage? most drinks on a single occasion?

menthlth
mental_data <- subset(brfss2013, menthlth <= 30)
quantile(mental_data$menthlth, c(.80,.85, .90, .95, .98, .99), na.rm=TRUE)
mental_data <- subset(brfss2013, menthlth <= 14)


ggplot(mental_data, aes(menthlth)) + stat_ecdf()
ggplot(mental_data, aes(menthlth, colour = sex)) + stat_ecdf()
ggplot(mental_data, aes(menthlth, colour = income2)) + stat_ecdf()
ggplot(mental_data, aes(menthlth, colour = imonth)) + stat_ecdf()


ggplot(mental_data, aes(menthlth, fill = sex)) +
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=1,
                   position="dodge",
                   colour="black",
                   na.rm=TRUE)

 ggplot(mental_data, aes(menthlth, fill = income2)) +
     geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                    binwidth=1,
                    position="dodge",
                    colour="black",
                    na.rm=TRUE)

ggplot(mental_data, aes(income2, menthlth)) + geom_boxplot()
ggplot(mental_data, aes(sex, menthlth)) + geom_boxplot()

ggplot(data = mental_data, aes(x = menthlth)) +
  geom_histogram(binwidth = 1)

ex_per_month_by_sex <- mental_data %>%
 group_by(imonth, sex) %>%
 summarise(
   median_ex = median(menthlth, na.rm=TRUE),
   mean_ex = mean(menthlth, na.rm=TRUE)
)
ggplot(data = ex_per_month_by_sex,aes(x = imonth, y = mean_ex, color=factor(sex)))+geom_line(aes(group=sex))
ggplot(data = ex_per_month_by_sex,aes(x = imonth, y = median_ex, color=factor(sex)))+geom_line(aes(group=sex))

ex_per_month_by_sex <- mental_data %>%
 group_by(imonth, income2) %>%
 summarise(
   median_ex = median(menthlth, na.rm=TRUE),
   mean_ex = mean(menthlth, na.rm=TRUE)
)
ggplot(data = ex_per_month_by_sex,aes(x = imonth, y = mean_ex, color=factor(income2)))+geom_line(aes(group=income2))
ggplot(data = ex_per_month_by_sex,aes(x = imonth, y = median_ex, color=factor(menthlth)))+geom_line(aes(group=menthlth))


ex_per_month_by_sex <- mental_data %>%
 group_by(income2) %>%
 summarise(
   median_ex = median(menthlth, na.rm=TRUE),
   mean_ex = mean(menthlth, na.rm=TRUE)
)
ggplot(data = ex_per_month_by_sex,aes(x = income2, y = mean_ex, color=factor(income2)))+geom_bar(aes(group=income2))
ggplot(data = ex_per_month_by_sex,aes(x = incom2, y = median_ex, color=factor(income2)))+geom_line(aes(group=income2))



* * *

## Part 3: Exploratory data analysis

NOTE: Insert code chunks as needed by clicking on the "Insert a new code chunk"
button (green button with orange arrow) above. Make sure that your code is visible
in the project you submit. Delete this note when before you submit your work.

**Research quesion 1:**

```{r}

```



**Research quesion 2:**

```{r}

```



**Research quesion 3:**

```{r}

```
