library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(infer)
library(PerformanceAnalytics)
library(stats)
library(robustHD)

load("~/Desktop/Coursera Project/data.Rdata")

##I thnk I'd like to explore how individuals thoughts on blacks and aids to blacks varies by education,geography, work status, class,and mediia exposure.
## I'd also like to know how these trends look across time.

# lets begin by looking at how responses to   uestions around black inferiority have changed across time
## are differences due to discrimination?
q1 <- gss %>% group_by(year, race, racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(year,race) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')
ggplot(data=q1, aes(x=year, y=share, fill=race, colour=race)) + geom_line() + facet_wrap(~race) + ggtitle('Share of Surveyed Who Believe Negative Life Outcomes for African-Americans are due to Discrimination') + ylab('% share replying yes') + theme(legend.position="bottom", legend.title = element_blank())

## interesting. less people are answering yes to the question "are differences due to discrimintaion"
## it also looks like this is independent of gender.
## steepest declines are within teh black population, although gradual dropff is occuring for whites.


## nest question.
q2 <- gss %>% group_by(year, race, sex, racdif2) %>% summarise(counts=n()) %>% filter(is.na(racdif2) != TRUE)
q2 <- q2 %>% group_by(year,race, sex) %>% mutate(share = counts/sum(counts, is.na=TRUE))
ggplot(data=q2, aes(x=year, y=share, fill=race, colour=race)) + geom_line() + facet_wrap(~sex+race)

## nest question. differences due to lack of education?
q3 <- gss %>% group_by(year, race, sex, racdif3) %>% summarise(counts=n()) %>% filter(is.na(racdif3) != TRUE)
q3 <- q3 %>% group_by(year,race, sex) %>% mutate(share = counts/sum(counts, is.na=TRUE))
ggplot(data=q3, aes(x=year, y=share, fill=race, colour=race)) + geom_line() + facet_wrap(~sex+race)

## next questionl. differences due to lack of will?
q4 <- gss %>% group_by(year, race, sex, racdif4) %>% summarise(counts=n()) %>% filter(is.na(racdif4) != TRUE)
q4 <- q4 %>% group_by(year,race, sex) %>% mutate(share = counts/sum(counts, is.na=TRUE))
ggplot(data=q4, aes(x=year, y=share)) + geom_line() + facet_wrap(~sex+race)

## lets focus on this first question, that seems to show some interesting trends.
## first, lets created some CIs.
## since we have low sample for the 'other' race, lets just focus on blacks vs whites.
## as a result, it looks like all of our counts > 10, so we dont have to use a simulation to create the CI.

q1_alt <- subset(q1, race != 'Other')

q1_alt <- q1_alt %>% mutate(se = sqrt(share*(1-share)/counts))
q1_alt <- q1_alt %>% mutate(ci_lower = share-(1.96*se), ci_upper = share+(1.96*se))

## now, lets reshape the data for easier graphing.
q1_mean <- q1_alt %>% select(year,race,sex,value=share)
q1_mean$sample <- 'mean'

q1_upper <- q1_alt %>% select(year,race,sex,value=ci_upper)
q1_upper$sample <- 'upper'

q1_lower <- q1_alt %>% select(year,race,sex,value=ci_lower)
q1_lower$sample <- 'lower'

q1_long <- rbind(q1_mean,q1_lower,q1_upper)

ggplot(data=q1_long, aes(x=year, y=value, colour=sample)) + geom_line() + facet_wrap(~sex+race)
## looks like we cant say much about the change beinig significantly different from prior years.
## what about testing if views across races differ?

q1 <- gss %>% group_by(year, race, racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(year,race) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')
ggplot(data=q1, aes(x=year, y=share, fill=race, colour=race)) + geom_line()

q1_alt <- subset(q1, race != 'Other')

q1_alt <- q1_alt %>% mutate(se = sqrt(share*(1-share)/counts))
q1_alt <- q1_alt %>% mutate(ci_lower = share-(1.96*se), ci_upper = share+(1.96*se))

## now, lets reshape the data for easier graphing.
q1_mean <- q1_alt %>% select(year,race,value=share)
q1_mean$sample <- 'mean'

q1_upper <- q1_alt %>% select(year,race,value=ci_upper)
q1_upper$sample <- 'upper'

q1_lower <- q1_alt %>% select(year,race,value=ci_lower)
q1_lower$sample <- 'lower'

q1_long <- rbind(q1_mean,q1_lower,q1_upper)

ggplot(data=q1_long, aes(x=year, y=value, colour=sample)) + geom_line() + facet_wrap(~race)
## it looks like the answer to this question is dependent on race.

## okay, lets try to break this down a bit.
## does class explain some of this?
## lets also only look at the year 2000 and on, so that we can have some decent sample size. we can do thiis because it looks like there isn't a great temporal effect

gss2000 <- subset(gss, year >= 2000)
q1 <- gss2000 %>% group_by(race, degree,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(race,degree) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')


q1_alt <- subset(q1, race != 'Other')

q1_alt <- q1_alt %>% mutate(se = sqrt(share*(1-share)/counts))
q1_alt <- q1_alt %>% mutate(ci_lower = share-(1.96*se), ci_upper = share+(1.96*se))

ggplot(data=q1_alt, aes(x=degree, y=share)) + geom_bar(stat='identity') + facet_wrap(~race) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

ggplot(data=q1_alt, aes(x=race, y=share)) + geom_bar(stat='identity') + facet_wrap(~degree) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

## work status?

q1 <- gss2000 %>% group_by(race, wrkstat,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(race,wrkstat) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')


q1_alt <- subset(q1, race != 'Other')

q1_alt <- q1_alt %>% mutate(se = sqrt(share*(1-share)/counts))
q1_alt <- q1_alt %>% mutate(ci_lower = share-(1.96*se), ci_upper = share+(1.96*se))

ggplot(data=q1_alt, aes(x=wrkstat, y=share)) + geom_bar(stat='identity') + facet_wrap(~race) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

ggplot(data=q1_alt, aes(x=race, y=share)) + geom_bar(stat='identity') + facet_wrap(~wrkstat) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

## party id?


q1 <- gss2000 %>% group_by(race, partyid,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(race,partyid) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')


q1_alt <- subset(q1, race != 'Other')

q1_alt <- q1_alt %>% mutate(se = sqrt(share*(1-share)/counts))
q1_alt <- q1_alt %>% mutate(ci_lower = share-(1.96*se), ci_upper = share+(1.96*se))


q1_white <- subset(q1_alt, race == 'White')
ggplot(data=q1_white, aes(x=partyid, y=share)) + geom_bar(stat='identity') + facet_wrap(~race) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

q1_black <- subset(q1_alt, race == 'Black')
ggplot(data=q1_black, aes(x=partyid, y=share)) + geom_bar(stat='identity') + facet_wrap(~race) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

ggplot(data=q1_alt, aes(x=race, y=share)) + geom_bar(stat='identity') + facet_wrap(~partyid) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

## religion?

q1 <- gss2000 %>% group_by(race, relig,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(race,relig) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')


q1_alt <- subset(q1, race != 'Other')

q1_alt <- q1_alt %>% mutate(se = sqrt(share*(1-share)/counts))
q1_alt <- q1_alt %>% mutate(ci_lower = share-(1.96*se), ci_upper = share+(1.96*se))


q1_white <- subset(q1_alt, race == 'White')
q1_white <- subset(q1_alt, counts >= 10)
ggplot(data=q1_white, aes(x=relig, y=share)) + geom_bar(stat='identity') + facet_wrap(~race) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

q1_black <- subset(q1_alt, race == 'Black')
ggplot(data=q1_black, aes(x=relig, y=share)) + geom_bar(stat='identity') + facet_wrap(~race) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

ggplot(data=q1_alt, aes(x=race, y=share)) + geom_bar(stat='identity') + facet_wrap(~relig) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)


z_star_95 = 1.96

q1 <- gss2000 %>% rep_sample_n(size = 18000, reps = 100, replace = FALSE) %>% group_by(race, relig,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(race,relig) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- q1 %>% mutate(se = sqrt(share*(1-share)/counts))
q1 <- q1 %>% mutate(ci_lower = share-(1.96*se), ci_upper = share+(1.96*se))

q1_white <- subset(q1, race == 'White')
q1_white <- subset(q1, counts >= 10)
ggplot(data=q1_white, aes(x=relig, y=share)) + geom_bar(stat='identity') + facet_wrap(~race) + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)


## estimateing ci diff between two proportions...lets do religion first
q1 <- gss2000 %>% group_by(race, relig,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(race,relig) %>% mutate(group_sum = sum(counts))
q1 <- q1 %>% group_by(race,relig) %>% mutate(share = counts/group_sum)
q1 <- subset(q1, racdif1 == 'Yes')

q1_a <- q1 %>% select(p1 = share, n1 = group_sum, relig, race) %>% filter(race == 'White', is.na(relig) != 'TRUE')
q1_b <- q1 %>% select(p2 = share, n2 = group_sum, relig, race) %>% filter(race == 'Black', is.na(relig) != 'TRUE')

q1 <- inner_join(q1_a,q1_b,by="relig")
q1 <- q1 %>% mutate(se1 = (p1*(1-p1))/n1, se2 = (p2*(1-p2))/n2)
q1 <- q1 %>% mutate(se = sqrt(se1+se2))
q1 <- q1 %>% mutate(diff_upper = (p1-p2)+1.96*se)
q1 <- q1 %>% mutate(diff_lower = (p1-p2)-1.96*se)
q1 <- q1 %>% select(relig,p1,n1,p2,n2,se,diff_upper,diff_lower)
q1 <- subset(q1, n1 >= 10 & n2 >= 10)
q1 <- q1 %>% mutate(point_estimate = p1-p2)

ggplot(data=q1, aes(x=relig, y=point_estimate)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin = diff_lower, ymax = diff_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

## hypothesis test for these two using pooled proportion
q1 <- q1 %>% mutate(pooled_proportion = (p1*n1+p2*n2)/(n1+n2))
q1 <- q1 %>% mutate(pooled_se = sqrt((pooled_proportion*(1-pooled_proportion)/n1)+(pooled_proportion*(1-pooled_proportion)/n2)))

q1 <- q1 %>% mutate(z_stat = (diff-0)/pooled_se)
q1 <- q1 %>% mutate(pvalued_2sided = 2*pnorm(-abs(z_stat)))
## from this, we can conclude that there are significant differences across all religions between blacka nd whites resposne to q1.

## dont forget to check for adequate sample size. which in this case is pooled_proportion * n1 >= 10 and pp*n2 >= 10

q1 <- q1 %>% mutate(p1_test = n1*pooled_proportion, p2_test = n2*pooled_proportion)
q1 <- q1 %>% filter(p1_test >= 10 & p2_test >= 10)


## estimateing ci diff between two proportions...degreeation levels now
q1 <- gss2000 %>% group_by(race, degree,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(race,degree) %>% mutate(group_sum = sum(counts))
q1 <- q1 %>% group_by(race,degree) %>% mutate(share = counts/group_sum)
q1 <- subset(q1, racdif1 == 'Yes')

q1_a <- q1 %>% select(p1 = share, n1 = group_sum, degree, race) %>% filter(race == 'White', is.na(degree) != 'TRUE')
q1_b <- q1 %>% select(p2 = share, n2 = group_sum, degree, race) %>% filter(race == 'Black', is.na(degree) != 'TRUE')

q1 <- inner_join(q1_a,q1_b,by="degree")
q1 <- q1 %>% mutate(se1 = (p1*(1-p1))/n1, se2 = (p2*(1-p2))/n2)
q1 <- q1 %>% mutate(se = sqrt(se1+se2))
q1 <- q1 %>% mutate(diff_upper = (p1-p2)+1.96*se)
q1 <- q1 %>% mutate(diff_lower = (p1-p2)-1.96*se)
q1 <- q1 %>% select(degree,p1,n1,p2,n2,se,diff_upper,diff_lower)
q1 <- subset(q1, n1 >= 10 & n2 >= 10)
q1 <- q1 %>% mutate(point_estimate = p1-p2)

ggplot(data=q1, aes(x=degree, y=point_estimate)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin = diff_lower, ymax = diff_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

## hypothesis test for these two using pooled proportion
q1 <- q1 %>% mutate(pooled_proportion = (p1*n1+p2*n2)/(n1+n2))
q1 <- q1 %>% mutate(pooled_se = sqrt((pooled_proportion*(1-pooled_proportion)/n1)+(pooled_proportion*(1-pooled_proportion)/n2)))

q1 <- q1 %>% mutate(z_stat = (point_estimate-0)/pooled_se)
q1 <- q1 %>% mutate(pvalued_2sided = round(2*pnorm(-abs(z_stat)),digits=3))
## from this, we can conclude that there are significant differences across all degreeions between blacka nd whites resposne to q1.

## dont forget to check for adequate sample size. which in this case is pooled_proportion * n1 >= 10 and pp*n2 >= 10

q1 <- q1 %>% mutate(p1_test = n1*pooled_proportion, p2_test = n2*pooled_proportion)
q1 <- q1 %>% filter(p1_test >= 10 & p2_test >= 10)


## estimateing ci diff between two proportions...partyid
q1 <- gss2000 %>% group_by(race, partyid,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(race,partyid) %>% mutate(group_sum = sum(counts))
q1 <- q1 %>% group_by(race,partyid) %>% mutate(share = counts/group_sum)
q1 <- subset(q1, racdif1 == 'Yes')

q1_a <- q1 %>% select(p1 = share, n1 = group_sum, partyid, race) %>% filter(race == 'White', is.na(partyid) != 'TRUE')
q1_b <- q1 %>% select(p2 = share, n2 = group_sum, partyid, race) %>% filter(race == 'Black', is.na(partyid) != 'TRUE')

q1 <- inner_join(q1_a,q1_b,by="partyid")
q1 <- q1 %>% mutate(se1 = (p1*(1-p1))/n1, se2 = (p2*(1-p2))/n2)
q1 <- q1 %>% mutate(se = sqrt(se1+se2))
q1 <- q1 %>% mutate(diff_upper = (p1-p2)+1.96*se)
q1 <- q1 %>% mutate(diff_lower = (p1-p2)-1.96*se)
q1 <- q1 %>% select(partyid,p1,n1,p2,n2,se,diff_upper,diff_lower)
q1 <- subset(q1, n1 >= 10 & n2 >= 10)
q1 <- q1 %>% mutate(point_estimate = p1-p2)

ggplot(data=q1, aes(x=partyid, y=point_estimate)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin = diff_lower, ymax = diff_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

## hypothesis test for these two using pooled proportion
q1 <- q1 %>% mutate(pooled_proportion = (p1*n1+p2*n2)/(n1+n2))
q1 <- q1 %>% mutate(pooled_se = sqrt((pooled_proportion*(1-pooled_proportion)/n1)+(pooled_proportion*(1-pooled_proportion)/n2)))

q1 <- q1 %>% mutate(z_stat = (point_estimate-0)/pooled_se)
q1 <- q1 %>% mutate(pvalued_2sided = round(2*pnorm(-abs(z_stat)),digits=3))
## from this, we can conclude that there are significant differences across all partyidions between blacka nd whites resposne to q1.

## dont forget to check for adequate sample size. which in this case is pooled_proportion * n1 >= 10 and pp*n2 >= 10

q1 <- q1 %>% mutate(p1_test = n1*pooled_proportion, p2_test = n2*pooled_proportion)
q1 <- q1 %>% filter(p1_test >= 10 & p2_test >= 10)


## dont forget to check for adequate sample size. which in this case is pooled_proportion * n1 >= 10 and pp*n2 >= 10

q1 <- q1 %>% mutate(p1_test = n1*pooled_proportion, p2_test = n2*pooled_proportion)
q1 <- q1 %>% filter(p1_test >= 10 & p2_test >= 10)


## estimateing ci diff between two proportions...year
gss <- gss %>% mutate(sample = case_when(year >= 1996 ~ '>= 1996', year < 1996 ~ '< 1996'))

q1 <- gss %>% group_by(race, sample,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
q1 <- q1 %>% group_by(race,sample) %>% mutate(group_sum = sum(counts))
q1 <- q1 %>% group_by(race,sample) %>% mutate(share = counts/group_sum)
q1 <- subset(q1, racdif1 == 'Yes')

q1_a <- q1 %>% select(p1 = share, n1 = group_sum, sample, race) %>% filter(sample == '>= 1996', is.na(sample) != 'TRUE')
q1_b <- q1 %>% select(p2 = share, n2 = group_sum, sample, race) %>% filter(sample == '< 1996', is.na(sample) != 'TRUE')

q1 <- inner_join(q1_a,q1_b,by="race")
q1 <- q1 %>% mutate(se1 = (p1*(1-p1))/n1, se2 = (p2*(1-p2))/n2)
q1 <- q1 %>% mutate(se = sqrt(se1+se2))
q1 <- q1 %>% mutate(diff_upper = (p1-p2)+1.96*se)
q1 <- q1 %>% mutate(diff_lower = (p1-p2)-1.96*se)
q1 <- q1 %>% select(race,sample.x,p1,n1,p2,n2,se,diff_upper,diff_lower)
q1 <- subset(q1, n1 >= 10 & n2 >= 10)
q1 <- q1 %>% mutate(point_estimate = p1-p2)

ggplot(data=q1, aes(x=race, y=point_estimate)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin = diff_lower, ymax = diff_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1)

## hypothesis test for these two using pooled proportion
q1 <- q1 %>% mutate(pooled_proportion = (p1*n1+p2*n2)/(n1+n2))
q1 <- q1 %>% mutate(pooled_se = sqrt((pooled_proportion*(1-pooled_proportion)/n1)+(pooled_proportion*(1-pooled_proportion)/n2)))

q1 <- q1 %>% mutate(z_stat = (point_estimate-0)/pooled_se)
q1 <- q1 %>% mutate(pvalued_2sided = round(2*pnorm(-abs(z_stat)),digits=3))
## from this, we can conclude that there are significant differences across time, with a large decline beginning around 1996 for both groups.

## okay, it seems pretty clear that answers to this question are very dependent on just about any demographics.
## lets see if variation exists within whites using a chi square indepdence.
## first, lets test whites by party.

q1 <- gss2000 %>% group_by(race, partyid,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race == 'White', is.na(partyid) != TRUE)
q1 <- q1 %>% group_by(race) %>% mutate(whites_total = sum(counts))
q1 <- q1 %>% group_by(racdif1) %>% mutate(response_total = sum(counts))
q1 <- q1 %>% group_by(partyid) %>% mutate(party_total = sum(counts))
q1 <- q1 %>% mutate(response_share = response_total/whites_total)
q1 <- q1 %>% mutate(party_share = counts/party_total)
q1 <- q1 %>% mutate(expected_total = response_share*party_total)
q1 <- q1 %>% select(race, partyid,racdif1,actual_total=counts, expected_total)

q1 <- q1 %>% mutate(squared_diff = ((actual_total-expected_total)^2)/expected_total)
q1 <- q1 %>% group_by() %>% mutate(chi_square = sum(squared_diff), degrees_of_freedom = 7)
q1 <- q1 %>% mutate(test_statistic = round(pchisq(chi_square,degrees_of_freedom,lower.tail=FALSE), digits=3))


## we cant conclude that party id is independent on party id?
