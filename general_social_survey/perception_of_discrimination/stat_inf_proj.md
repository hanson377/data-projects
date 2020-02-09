Quick Analysis on American’s perception of the impact of discrimination
on life outcomes
================

## Setup

### Load packages

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.6.1

``` r
library(dplyr)
```

### Load data

``` r
load("~/GitHub/data-projects/general_social_survey/perception_of_discrimination/data.Rdata")
```

## Research question

I’d like to explore the difference in black and white’s perception of
how discrimination impacts the life outcomes for black people in
America. I’d like to explore this across various demographics including
gender, degree attainment, religion, and political party affiliation.
I’d also like to examine how this perception has changed across time.

I’m am going to use the response from black and white’s to the following
question as an indicator for how people perceive discrimination impacts
life outcomes:

“On average, African-Americans have jobs, income, and housing than white
people. Do you think these differences are mainly due to
discrimination?”

-----

## Part 3: Exploratory data analysis

Before we can dig into the differences across certain demographics, lets
first simple examine how white and black’s responses to this question
have changed across
time.

-----

``` r
q1 <- gss %>% group_by(year, race, racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
```

    ## Warning: Factor `racdif1` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

``` r
q1 <- q1 %>% group_by(year,race) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')
ggplot(data=q1, aes(x=year, y=share, fill=race, colour=race)) + geom_line() + facet_wrap(~race) + ggtitle('Share of Surveyed Who Believe Negative Life Outcomes for African-Americans are due to Discrimination') + ylab('% share replying yes') + theme(legend.position="bottom", legend.title = element_blank())
```

![](stat_inf_proj_files/figure-gfm/time-series%20black%20vs%20white-1.png)<!-- -->

It seems answers to this question have shifted across time with 1994
being the time in which this decreased. Although we saw a decline across
both races, the decline seems to be more substantial for
African-Americans. It is not clear if this is a significant change. We
will test this later.

For now, lets look at some shifts within demographics across time for
African-Americans and Whites. Let us generate views for the following
demographics, one-by-one, comparing responses prior to 1996 and after
1996:

1.  Gender
2.  Highest Degree of Attainment
3.  Political Party
Affiliation
4.  Religion

<!-- end list -->

``` r
gss <- gss %>% mutate(sample = case_when(year >= 1996 ~ '>= 1996', year < 1996 ~ '< 1996'))

q1 <- gss %>% group_by(sample, race, sex, racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other', is.na(sex) != TRUE)
```

    ## Warning: Factor `racdif1` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

``` r
q1 <- q1 %>% group_by(sample,race,sex) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')
ggplot(data=q1, aes(x=sample, y=share, fill=sex, colour=sex)) + geom_bar(stat='identity', position='dodge') + facet_wrap(~race) + ggtitle('Share of Surveyed Who Believe Negative Life Outcomes for African-Americans are due to Discrimination') + ylab('% share replying yes') + theme(legend.position="bottom", legend.title = element_blank())
```

![](stat_inf_proj_files/figure-gfm/gender%20x%20race-1.png)<!-- -->

``` r
q1 <- gss %>% group_by(sample, race, degree, racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other', is.na(degree) != TRUE)
```

    ## Warning: Factor `degree` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

    ## Warning: Factor `racdif1` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

``` r
q1 <- q1 %>% group_by(sample,race,degree) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')
ggplot(data=q1, aes(x=sample, y=share, fill=degree, colour=degree)) + geom_bar(stat='identity', position='dodge') + facet_wrap(~race) + ggtitle('Share of Surveyed Who Believe Negative Life Outcomes for African-Americans are due to Discrimination') + ylab('% share replying yes') + theme(legend.position="bottom", legend.title = element_blank())
```

![](stat_inf_proj_files/figure-gfm/race%20x%20degree-1.png)<!-- -->

``` r
q1 <- gss %>% group_by(sample, race, partyid, racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other', is.na(partyid) != TRUE)
```

    ## Warning: Factor `partyid` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

    ## Warning: Factor `racdif1` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

``` r
q1 <- q1 %>% group_by(sample,race,partyid) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')
ggplot(data=q1, aes(x=sample, y=share, fill=partyid, colour=partyid)) + geom_bar(stat='identity', position='dodge') + facet_wrap(~race) + ggtitle('Share of Surveyed Who Believe Negative Life Outcomes for African-Americans are due to Discrimination') + ylab('% share replying yes') + theme(legend.position="bottom", legend.title = element_blank())
```

![](stat_inf_proj_files/figure-gfm/race%20x%20partyid-1.png)<!-- -->

``` r
q1 <- gss %>% group_by(sample, race, relig, racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other', is.na(relig) != TRUE)
```

    ## Warning: Factor `relig` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

    ## Warning: Factor `racdif1` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

``` r
q1 <- q1 %>% group_by(sample,race,relig) %>% mutate(share = counts/sum(counts, is.na=TRUE))
q1 <- subset(q1, racdif1 == 'Yes')
ggplot(data=q1, aes(x=sample, y=share, fill=relig, colour=relig)) + geom_bar(stat='identity', position='dodge') + facet_wrap(~race) + ggtitle('Share of Surveyed Who Believe Negative Life Outcomes for African-Americans are due to Discrimination') + ylab('% share replying yes') + theme(legend.position="bottom", legend.title = element_blank())
```

![](stat_inf_proj_files/figure-gfm/race%20x%20relig-1.png)<!-- -->

From the above, it seems pretty clear that just about any demographic we
are interested in within each race saw a decline after 1996 in those
responding ‘yes’.

I now want to answer a few questions:

1.  is the decline after 1996 significant for both races?
2.  is the difference between races significant?
3.  if different in aggregate, is this share dependent on political
    affiliation for white individuals?

## Part 4: Inference

Lets begin by calculate the confidence interval for the difference in
the share replying ‘yes’ for each race before and after 1996.

First, we must check if we meet all of the conditions required for
calculating the CIs for two proportions.

1.  success-failure
conditions
2.  indepedence

<!-- end list -->

``` r
q1 <- gss %>% group_by(race, sample,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
```

    ## Warning: Factor `racdif1` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

``` r
q1 <- q1 %>% group_by(race,sample) %>% mutate(group_total = sum(counts))
q1 <- q1 %>% group_by(race,sample) %>% mutate(share = counts/group_total)

q1
```

    ## # A tibble: 8 x 6
    ## # Groups:   race, sample [4]
    ##   race  sample  racdif1 counts group_total share
    ##   <fct> <chr>   <fct>    <int>       <int> <dbl>
    ## 1 White < 1996  Yes       3715        9678 0.384
    ## 2 White < 1996  No        5963        9678 0.616
    ## 3 White >= 1996 Yes       3203        9952 0.322
    ## 4 White >= 1996 No        6749        9952 0.678
    ## 5 Black < 1996  Yes        910        1161 0.784
    ## 6 Black < 1996  No         251        1161 0.216
    ## 7 Black >= 1996 Yes       1137        1891 0.601
    ## 8 Black >= 1996 No         754        1891 0.399

We can see from the above that our count columns imply than we have more
than 10 for both the yes and no condition by race. This means the
success-failure condition is satisfied.

For independence, I think it is safe to assume that white and black
individuals are independent of each-other. I think it is also safe to
assume that every year’s survey result is independent from the other.

Thus, we can carry forward with our CI.

``` r
q1 <- subset(q1, racdif1 == 'Yes')
q1_a <- q1 %>% select(p1 = share, n1 = group_total, sample, race)
q1_a <- q1_a %>% filter(sample == '>= 1996', is.na(sample) != 'TRUE')
q1_b <- q1 %>% select(p2 = share, n2 = group_total, sample, race)
q1_b <- q1_b %>% filter(sample == '< 1996', is.na(sample) != 'TRUE')

q1 <- inner_join(q1_a,q1_b,by="race")
q1 <- q1 %>% mutate(se1 = (p1*(1-p1))/n1, se2 = (p2*(1-p2))/n2)
q1 <- q1 %>% mutate(se = sqrt(se1+se2))
q1 <- q1 %>% mutate(diff_upper = (p1-p2)+1.96*se)
q1 <- q1 %>% mutate(diff_lower = (p1-p2)-1.96*se)
q1 <- q1 %>% select(race,sample.x,p1,n1,p2,n2,se,diff_upper,diff_lower)
q1 <- subset(q1, n1 >= 10 & n2 >= 10)
q1 <- q1 %>% mutate(point_estimate = p1-p2)
table <- q1 %>% select(race, point_estimate, diff_lower, diff_upper)
```

    ## Adding missing grouping variables: `sample.x`

``` r
table
```

    ## # A tibble: 2 x 5
    ## # Groups:   race, sample.x [2]
    ##   sample.x race  point_estimate diff_lower diff_upper
    ##   <chr>    <fct>          <dbl>      <dbl>      <dbl>
    ## 1 >= 1996  White        -0.0620    -0.0754    -0.0487
    ## 2 >= 1996  Black        -0.183     -0.215     -0.150

``` r
ggplot(data=q1, aes(x=race, y=point_estimate)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin = diff_lower, ymax = diff_upper),width = 0.2,linetype = "dotted",position = position_dodge(width = 0.5), color="red", size=1) + ggtitle('Difference Before and After 1996 in Those Responding Yes to Survey') + ylab('difference in proportion across time periods') + theme(legend.position="bottom", legend.title = element_blank())
```

![](stat_inf_proj_files/figure-gfm/test%20test-1.png)<!-- -->

It is clear that our confidence intervals imply that we might have a
significant difference here. But let us perform a hypothesis test to do
this.

First, we must make sure we fulfill the requirements for a hypothesis
test. Although the indepdence condition is the same for both CIs and
Hypothesis tests, we must slightly change our test for the
success-failure condition. We must calculate the pooled proportion and
ensure that the pooled proportion times the sample size for each group
is greater than 10.

``` r
q1 <- q1 %>% mutate(pooled_proportion = (p1*n1+p2*n2)/(n1+n2))
q1 <- q1 %>% mutate(test_n1 = pooled_proportion*n1, test_n2 = pooled_proportion*n2)
table <- q1 %>% select(race, test_n1, test_n2)
```

    ## Adding missing grouping variables: `sample.x`

``` r
table
```

    ## # A tibble: 2 x 4
    ## # Groups:   race, sample.x [2]
    ##   sample.x race  test_n1 test_n2
    ##   <chr>    <fct>   <dbl>   <dbl>
    ## 1 >= 1996  White   3507.   3411.
    ## 2 >= 1996  Black   1268.    779.

Perfect. Our tests all have a value greater than 10. This means we can
carry on with our hypothesis test.

Let us state our null and alternative hypotheses.

H0: There is not a significant difference in the proportion of both
blacks and whites replying yes to the discrimination question before and
after 1996. (P After 1996 = P Before 1996) HA: The proportion of both
blacks and whites replying ‘yes’ to the discrimination question after
1996 is less than prior to 1996. (P After 1996 \< P Before
1996)

``` r
q1 <- q1 %>% mutate(pooled_se = sqrt((pooled_proportion*(1-pooled_proportion)/n1)+(pooled_proportion*(1-pooled_proportion)/n2)))
q1 <- q1 %>% mutate(pooled_proportion = (p1*n1+p2*n2)/(n1+n2))
q1 <- q1 %>% mutate(pooled_se = sqrt((pooled_proportion*(1-pooled_proportion)/n1)+(pooled_proportion*(1-pooled_proportion)/n2)))

q1 <- q1 %>% mutate(z_stat = (point_estimate-0)/pooled_se)
q1 <- q1 %>% mutate(p_value_one_sided = round(pnorm(abs(z_stat), lower.tail = FALSE), digits=3))

q1$z_stat
```

    ## [1]  -9.093075 -10.417541

``` r
q1$p_value_one_sided
```

    ## [1] 0 0

Our p-value for both races is near 0. Thus, we reject the null
hypothesis and conclude that, for both blacks and whites, the difference
in the proportion of both blacks and whites replying ‘yes’ to the
discrimination question before and after 1996 is significantly
different.

Now that we know this, lets focus on the period closest to us for
answering the question “Do a greater proportion of blacks think that
poor life outcomes are driven by discrimination compared to whites?”

We will look at the proportion across races for all responses after
1995.

Since we’ve already established the conditions for constructing a CI
interval above, lets go ahead and just calculate the CI, infer if we
think the hypothesis test will show a difference, and then run the
hypothesis test.

``` r
gss1996 <- subset(gss, year >= 1996)

q1 <- gss1996 %>% group_by(race,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race != 'Other')
```

    ## Warning: Factor `racdif1` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

``` r
q1 <- q1 %>% group_by(race) %>% mutate(group_sum = sum(counts))
q1 <- q1 %>% group_by(race) %>% mutate(share = counts/group_sum)
q1 <- q1 %>% mutate(sample = 'black vs white') %>% filter(racdif1 == 'Yes')

q1_a <- q1 %>% select(p1 = share, n1 = group_sum, sample, race) %>% filter(race == 'White', is.na(sample) != 'TRUE')
q1_b <- q1 %>% select(p2 = share, n2 = group_sum, sample, race) %>% filter(race == 'Black', is.na(sample) != 'TRUE')

q1 <- inner_join(q1_a,q1_b,by="sample")
q1 <- q1 %>% mutate(se1 = (p1*(1-p1))/n1, se2 = (p2*(1-p2))/n2)
q1 <- q1 %>% mutate(se = sqrt(se1+se2))
q1 <- q1 %>% mutate(diff_upper = (p1-p2)+1.96*se)
q1 <- q1 %>% mutate(diff_lower = (p1-p2)-1.96*se)
q1 <- q1 %>% select(sample,p1,n1,p2,n2,se,diff_upper,diff_lower)
```

    ## Adding missing grouping variables: `race.x`

``` r
q1 <- subset(q1, n1 >= 10 & n2 >= 10)
q1 <- q1 %>% mutate(point_estimate = p1-p2)

table <- q1 %>% select(sample, point_estimate,diff_lower, diff_upper)
```

    ## Adding missing grouping variables: `race.x`

``` r
table
```

    ## # A tibble: 1 x 5
    ## # Groups:   race.x [1]
    ##   race.x sample         point_estimate diff_lower diff_upper
    ##   <fct>  <chr>                   <dbl>      <dbl>      <dbl>
    ## 1 White  black vs white         -0.279     -0.303     -0.256

It seems pretty clear that the difference in proportions for blacks and
whites is substantial and potentially significant. Lets run the test to
confirm.

``` r
q1 <- q1 %>% mutate(pooled_proportion = (p1*n1+p2*n2)/(n1+n2))
q1 <- q1 %>% mutate(test_n1 = pooled_proportion*n1, test_n2 = pooled_proportion*n2)
table <- q1 %>% select(sample, test_n1, test_n2)
```

    ## Adding missing grouping variables: `race.x`

``` r
table
```

    ## # A tibble: 1 x 4
    ## # Groups:   race.x [1]
    ##   race.x sample         test_n1 test_n2
    ##   <fct>  <chr>            <dbl>   <dbl>
    ## 1 White  black vs white   3647.    693.

Above, we can see that both proportions have n \>= 10. We can also
deduce that all of our data points are independent, as they are from
different survey years.

Let us state our null and alternative hypotheses.

H0: There is not a significant difference in the proportion of blacks
and whites replying yes to the discrimination question. (P black = P
white) HA: There IS a significant difference in the proportion of blacks
and whites replying yes to the discrimination question. (P black \!= \<
P
white)

``` r
q1 <- q1 %>% mutate(pooled_se = sqrt((pooled_proportion*(1-pooled_proportion)/n1)+(pooled_proportion*(1-pooled_proportion)/n2)))

q1 <- q1 %>% mutate(z_stat = (point_estimate-0)/pooled_se)
q1 <- q1 %>% mutate(p_value_one_sided = 2*pnorm(-abs(z_stat)))

q1$z_stat
```

    ## [1] -23.11712

``` r
q1$p_value_one_sided
```

    ## [1] 3.115119e-118

From the above, we find a test-statistic of -23 and a p-score of nearly
zero. There is strong evidence that there exists a significant
difference in the proportion of blacks attributing poor life outcomes to
discrimination relative to whites.

This is very interesting. But lets do one quick test to see if there are
any demographics within the white race that have views independent of
what is expected.

Let us examine political affiliation using a chi-square independence
test. Let us being by stating our hypothesis.

H0: Political affiliation and the share of those attributing poor life
outcomes for blacks to discrimination are independent. H1: Political
affiliation and the share of those attributing poor life outcomes for
blacks to discrimination are dependent.

Let us now run through our conditions for running a chi-square
independence test.

1.  independence

<!-- end list -->

1.  random sample/assignment: yes, as discussed earlier, this survey is
    a random sample.

<!-- end list -->

2.  sample size: each particular case must have at least five cells:
    also yes, as we have 8 political affiliations per yes/no response.

And with that, we can run our
test\!

``` r
q1 <- gss1996 %>% group_by(race, partyid,racdif1) %>% summarise(counts=n()) %>% filter(is.na(racdif1) != TRUE, race == 'White', is.na(partyid) != TRUE)
```

    ## Warning: Factor `partyid` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

    ## Warning: Factor `racdif1` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

``` r
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
q1$chi_square
```

    ##  [1] 360.4137 360.4137 360.4137 360.4137 360.4137 360.4137 360.4137 360.4137
    ##  [9] 360.4137 360.4137 360.4137 360.4137 360.4137 360.4137 360.4137 360.4137

``` r
q1$test_statistic
```

    ##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

From the above, it is clear that we reject the hull hypothesis.
Political affiliation and the share of white people attributing poor
life outcomes for blacks to discrimination are dependent.

So, in summary, we have learned three things today: a. The share of
individuals who believe discrimination impacts the life outcomes of
blacks has shifted across time with a significant decline occurring
after 1996 for both black and whites. b. Since 1996, the share of
individuals who believe discrimination impacts the life outcomes of
blacks is significantly different for whites and blacks, with blacks
more likely to believe that discrimination is the main driver of poor
life outcomes for blacks. c. Although this significant difference
exists, within the white population, political party and this share of
people are dependent. This means that there are groups of political
affiliations that differ unexpectedly from the general population.
