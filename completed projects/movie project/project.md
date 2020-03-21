Modeling and prediction for movies
================

## Setup

### Load packages

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.6.1

``` r
library(dplyr)
library(statsr)
```

    ## Warning: package 'statsr' was built under R version 3.6.2

    ## Warning: package 'BayesFactor' was built under R version 3.6.2

    ## Warning: package 'coda' was built under R version 3.6.2

``` r
library(ggcorrplot)
```

    ## Warning: package 'ggcorrplot' was built under R version 3.6.2

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.6.2

### Load data

``` r
load("~/GitHub/data-projects/movie project/movies.Rdata")
```

-----

## Part 1: Data

The data here is a random sample of movies created before 2016. This
implies we can likely generalize trends and relationships found in our
analysis to the broader population of all movies before 2016.

However, I’m concerned that the data here is a bit imbalanced across
time and identifying a time period for which we can generalize our
results across is a bit difficult. For example, our data only goes back
to 1970. This means that, at best, we can only generalize our results
from 1970 forward.

Furthermore, if we have more movies existing in more recent years than
previous years, I’d wonder how much we can actually generalize what we
find to the entire population of movies between 1970 and 2016. It seems
to be the case that the number of movies in the dataset per year
increases across time. For example, we have one movie listed in 1970 and
26 in 2012. This leads me to believe that it is somewhat unlikely we can
actually generalize our results to the general population of movies
between 1970 and 2016. However, for the purpose of this exercise, we
will assume we can.

Additionally, this data exists outside of an experimental setting, and
thus causal relationships cannot be made.

-----

## Part 2: Research question

First, I’d like to understand if critics scores deviate from that of the
audience. If so, in what direction? Are critics more likely to rate
movies lower than the audience? If so, is there anything that we can
find to explain these deviations?

I’d then like to build a model that can predict how different a critics
score will be from the audience.

-----

## Part 3: Exploratory data analysis

First, lets get a general feel for what the data looks like.

Namely, I’m interested in understanding the time-series element of this
dataset. I’d like to understand how our movie volume changes across time
and then understand if the average rating has changed across time. I’d
also like to understand how the scores across our different categories
(imdb, audience, critic) correlate and/or differ across time.

The answers to these questions will greatly inform our answer to our
research question. It will help us also understand if we need to add
some temporal component to our predictive model at the end of this
exercise.

First, let us look at the volume of movies across
time.

``` r
ts_summary <- movies %>% group_by(thtr_rel_year) %>% summarise(movies = n_distinct(title),
                                                              mean_imdb_rating = mean(imdb_rating),
                                                              median_imdb_rating = median(imdb_rating),
                                                              mean_critic_score = mean(critics_score),
                                                              median_critic_score = median(critics_score),
                                                              mean_audience_score = mean(audience_score),
                                                              median_audience_score = median(audience_score))

imdb <- ts_summary %>% select(thtr_rel_year, mean = mean_imdb_rating, median = median_imdb_rating)
imdb$sample <- 'imdb score'

critic <- ts_summary %>% select(thtr_rel_year, mean = mean_critic_score, median = median_critic_score)
critic$sample <- 'critic score'

audience <- ts_summary %>% select(thtr_rel_year, mean = mean_audience_score, median = median_audience_score)
audience$sample <- 'audience score'

summary <- rbind(critic,audience)
summary$mean <- round(summary$mean/10,digits=2)
summary$median <- round(summary$median/10, digits=2)

summary <- rbind(summary,imdb)

ggplot(ts_summary, aes(x=thtr_rel_year, y=movies)) + geom_line() + theme(legend.position="bottom", legend.title = element_blank())
```

![](project_files/figure-gfm/quick%20survey%20of%20the%20data-1.png)<!-- -->

``` r
ggplot(summary, aes(x=thtr_rel_year, y=median, color=sample)) + geom_line() + theme(legend.position="bottom", legend.title = element_blank())
```

![](project_files/figure-gfm/quick%20survey%20of%20the%20data-2.png)<!-- -->

``` r
ggplot(summary, aes(x=thtr_rel_year, y=mean, color=sample)) + geom_line() + theme(legend.position="bottom", legend.title = element_blank())
```

![](project_files/figure-gfm/quick%20survey%20of%20the%20data-3.png)<!-- -->

We’ve learned a few important things from above.

First, as I suspected, I’m not sure our sample is representative across
time. We seem to have much more movies in our data from recent years
than older years.

Secondly, we can see that outside of the first few years of data,
audience and imdb scores are both correlative and mostly stable across
time.

Thirdly, critics scores seem to not be correlative nor similar to
imdb/audience scores as of late. Since 1995 or so, we’ve seen
audience/imdb scores diverge from that of critics scores. To simply our
analysis, we are going to exclude movies released in theatre before 1995
from our analysis.

It also seems to be that median/mean scores aren’t incredibly different.
This means we might not have too many outliers when summarizing by year.

Okay, now we that understand some basics of the data, lets begin
exploring our research question. First, lets look at how much a critic’s
score and an audience’s score deviate. Lets look at the distribution of
this deviation.

``` r
movies <- subset(movies, thtr_rel_year >= 2000)
movies <- movies %>% mutate(diff = (critics_score-audience_score))
summary <- movies %>% summarise(mean_diff = mean(diff), median_diff = median(diff))
summary
```

    ## # A tibble: 1 x 2
    ##   mean_diff median_diff
    ##       <dbl>       <dbl>
    ## 1     -5.58          -3

``` r
ggplot(data=movies, aes(x=diff)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](project_files/figure-gfm/deviation%20exploration-1.png)<!-- -->

``` r
upper_diff = quantile(movies$diff, .975)
lower_diff = quantile(movies$diff, .025)

movies <- subset(movies, diff > lower_diff & diff < upper_diff)

ggplot(data=movies, aes(x=diff)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](project_files/figure-gfm/deviation%20exploration-2.png)<!-- -->

From the above, we can see that this distribution is left-skewed. This
implies that a critic is more likely to rate a movie below the audience.
This is confirmed with the fact that the mean sits farther to the left
than the median.

For the next section of my analysis, I’m going to attempt to identify
other continuous variables that are correlated to our differential
variable.

``` r
## audience score
ggplot(data=movies, aes(x=diff, y=audience_score)) + geom_point()
```

![](project_files/figure-gfm/deviation%20correlations-1.png)<!-- -->

``` r
lm <- lm(data=movies, diff ~ audience_score)
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ audience_score, data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -38.593 -11.255   1.851  13.360  35.869 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    -10.94300    3.12722  -3.499 0.000534 ***
    ## audience_score   0.09258    0.04782   1.936 0.053742 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.17 on 316 degrees of freedom
    ## Multiple R-squared:  0.01172,    Adjusted R-squared:  0.008596 
    ## F-statistic: 3.749 on 1 and 316 DF,  p-value: 0.05374

``` r
## imdb num votes (or popularity)
upper = quantile(movies$imdb_num_votes, .95)
lower = quantile(movies$imdb_num_votes, .05)
movies_alt <- subset(movies, imdb_num_votes < upper & imdb_num_votes > lower)

ggplot(data=movies_alt, aes(x=diff, y=imdb_num_votes)) + geom_point()
```

![](project_files/figure-gfm/deviation%20correlations-2.png)<!-- -->

``` r
lm <- lm(data=movies_alt, diff ~ imdb_num_votes)
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ imdb_num_votes, data = movies_alt)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -39.686 -12.563   2.175  14.298  34.583 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    -5.275e+00  1.341e+00  -3.933 0.000105 ***
    ## imdb_num_votes -1.252e-05  1.607e-05  -0.779 0.436451    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.68 on 284 degrees of freedom
    ## Multiple R-squared:  0.002134,   Adjusted R-squared:  -0.00138 
    ## F-statistic: 0.6073 on 1 and 284 DF,  p-value: 0.4365

``` r
## runtime
movies <- subset(movies, runtime > 40)
ggplot(data=movies, aes(x=diff, y=runtime)) + geom_point()
```

![](project_files/figure-gfm/deviation%20correlations-3.png)<!-- -->

``` r
lm <- lm(data=movies, diff ~ log(runtime))
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ log(runtime), data = movies)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -38.41 -12.29   1.69  13.19  35.60 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   -53.761     28.562  -1.882   0.0607 .
    ## log(runtime)   10.481      6.164   1.700   0.0901 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.2 on 315 degrees of freedom
    ## Multiple R-squared:  0.009095,   Adjusted R-squared:  0.005949 
    ## F-statistic: 2.891 on 1 and 315 DF,  p-value: 0.09006

``` r
## days between theatre release and dvd release
movies$thtr_date <- as.Date(with(movies, paste(thtr_rel_year, thtr_rel_month, thtr_rel_day,sep="-")), "%Y-%m-%d")
movies$dvd_date <- as.Date(with(movies, paste(dvd_rel_year, dvd_rel_month, dvd_rel_day,sep="-")), "%Y-%m-%d")
movies$date_diff <- as.numeric(difftime(movies$dvd_date,movies$thtr_date, units='weeks'))
summary(movies$date_diff)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ## -151.43   15.57   19.57   29.44   26.57  280.71       1

``` r
ggplot(data=movies, aes(x=date_diff)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1 rows containing non-finite values (stat_bin).

![](project_files/figure-gfm/deviation%20correlations-4.png)<!-- -->

``` r
movies <- subset(movies, date_diff >= 0) ## remove strange case where dvd release is before theatre release
ggplot(data=movies, aes(x=date_diff)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](project_files/figure-gfm/deviation%20correlations-5.png)<!-- -->

``` r
year <- movies %>% group_by(thtr_rel_year) %>% summarise(mean = mean(date_diff), median = median(date_diff))
ggplot(data = year, aes(x=thtr_rel_year, y = mean)) + geom_line(stat='identity')
```

![](project_files/figure-gfm/deviation%20correlations-6.png)<!-- -->

``` r
ggplot(data = year, aes(x=thtr_rel_year, y = median)) + geom_line(stat='identity')
```

![](project_files/figure-gfm/deviation%20correlations-7.png)<!-- -->

``` r
upper = quantile(movies$date_diff, .975)
lower = quantile(movies$date_diff, .025)
movies_alt <- subset(movies, date_diff < upper & date_diff > lower)

ggplot(data=movies_alt, aes(x=diff, y=date_diff)) + geom_point()
```

![](project_files/figure-gfm/deviation%20correlations-8.png)<!-- -->

``` r
lm <- lm(data=movies, diff ~ date_diff)
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ date_diff, data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -39.468 -11.563   2.452  13.626  34.532 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6.18207    1.35713  -4.555 7.52e-06 ***
    ## date_diff    0.03162    0.03130   1.010    0.313    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.22 on 311 degrees of freedom
    ## Multiple R-squared:  0.003271,   Adjusted R-squared:  6.567e-05 
    ## F-statistic:  1.02 on 1 and 311 DF,  p-value: 0.3132

From the above, we find an interesting trend shown by the scatterplot of
audience score and our critic score differential. As audience score
decreases, our differential also decreases. This implies that if we were
to use this in our predictive model, we would violate the assumption of
homoskedasticity, as the expected variation decreases as audience score
decreases. This trend makes sense, as the possible values our deviation
can increases and decreases as audience score fluctuates.

From the above, we also find that movie runtime is a significant
predictive power for explaining a negative differential, although pretty
small.

Now lets explore deviations across a few demographics. Namely, lets look
at MPAA rating, genre, release year, release
month.

``` r
genre <- movies %>% group_by(genre) %>% summarise(movies = n_distinct(title), median = median(diff), mean = mean(diff), iqr = IQR(diff))
genre
```

    ## # A tibble: 11 x 5
    ##    genre                     movies median   mean   iqr
    ##    <fct>                      <int>  <dbl>  <dbl> <dbl>
    ##  1 Action & Adventure            31  -14   -14.1  26   
    ##  2 Animation                      7  -10   -13.4  22.5 
    ##  3 Art House & International      8   -8    -6.25 32.5 
    ##  4 Comedy                        42   -9   -11.3  25.8 
    ##  5 Documentary                   38    5     3.10 12   
    ##  6 Drama                        138   -1    -4.26 27.8 
    ##  7 Horror                         8    0    -3    22.8 
    ##  8 Musical & Performing Arts      6   10     9.67  8.75
    ##  9 Mystery & Suspense            27   -6    -5.30 25.5 
    ## 10 Other                          4   -2.5  -2.5   2.5 
    ## 11 Science Fiction & Fantasy      3    2     2     1

``` r
ggplot(data=movies, aes(x=genre, y=diff)) + geom_boxplot()
```

![](project_files/figure-gfm/deviations%20by%20demographics-1.png)<!-- -->

``` r
lm <- lm(data=movies, diff ~ genre)
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ genre, data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -38.739 -12.714   1.897  12.261  42.129 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    -14.1290     2.9897  -4.726 3.52e-06 ***
    ## genreAnimation                   0.7005     6.9658   0.101  0.91997    
    ## genreArt House & International   7.8790     6.6011   1.194  0.23357    
    ## genreComedy                      2.8433     3.9415   0.721  0.47124    
    ## genreDocumentary                17.2316     4.0054   4.302 2.29e-05 ***
    ## genreDrama                       9.8682     3.3085   2.983  0.00309 ** 
    ## genreHorror                     11.1290     6.6011   1.686  0.09284 .  
    ## genreMusical & Performing Arts  23.7957     7.4243   3.205  0.00149 ** 
    ## genreMystery & Suspense          8.8327     4.3819   2.016  0.04471 *  
    ## genreOther                      11.6290     8.8437   1.315  0.18952    
    ## genreScience Fiction & Fantasy  16.1290    10.0649   1.603  0.11009    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.65 on 302 degrees of freedom
    ## Multiple R-squared:  0.09583,    Adjusted R-squared:  0.06589 
    ## F-statistic: 3.201 on 10 and 302 DF,  p-value: 0.0006277

``` r
mpaa_rating <- movies %>% group_by(mpaa_rating) %>% summarise(movies = n_distinct(title), median = median(diff), mean = mean(diff), iqr = IQR(diff))
mpaa_rating
```

    ## # A tibble: 5 x 5
    ##   mpaa_rating movies median   mean   iqr
    ##   <fct>        <int>  <dbl>  <dbl> <dbl>
    ## 1 G                5  -10   -11.4    6  
    ## 2 PG              38   -8.5  -9.03  27  
    ## 3 PG-13           82  -10    -9.69  30  
    ## 4 R              147   -3    -4.17  26  
    ## 5 Unrated         40    5     4.53  16.2

``` r
ggplot(data=movies, aes(x=mpaa_rating, y=diff)) + geom_boxplot()
```

![](project_files/figure-gfm/deviations%20by%20demographics-2.png)<!-- -->

``` r
lm <- lm(data=movies, diff ~ mpaa_rating)
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ mpaa_rating, data = movies)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -38.83 -11.83   1.17  13.17  33.17 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)         -11.400      7.481  -1.524   0.1286  
    ## mpaa_ratingPG         2.374      7.958   0.298   0.7657  
    ## mpaa_ratingPG-13      1.713      7.703   0.222   0.8241  
    ## mpaa_ratingR          7.230      7.607   0.950   0.3427  
    ## mpaa_ratingUnrated   15.925      7.935   2.007   0.0456 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.73 on 308 degrees of freedom
    ## Multiple R-squared:  0.0687, Adjusted R-squared:  0.0566 
    ## F-statistic:  5.68 on 4 and 308 DF,  p-value: 0.0002012

``` r
year <- movies %>% group_by(thtr_rel_year) %>% summarise(movies = n_distinct(title), median = median(diff), mean = mean(diff), iqr = IQR(diff))
year
```

    ## # A tibble: 15 x 5
    ##    thtr_rel_year movies median    mean   iqr
    ##            <dbl>  <int>  <dbl>   <dbl> <dbl>
    ##  1          2000     18   -5.5  -8.56   28.2
    ##  2          2001     17   -7    -7.71   19  
    ##  3          2002     23    0    -2.04   22  
    ##  4          2003     22   -8.5  -9.27   15.2
    ##  5          2004     24  -10   -13.7    32.8
    ##  6          2005     16   -2    -5.75   27.5
    ##  7          2006     31  -12   -10.5    20.5
    ##  8          2007     29    1    -6.03   27  
    ##  9          2008     18    3    -4.16   35  
    ## 10          2009     18    0.5  -3.94   21.5
    ## 11          2010     20    5.5   4.5    13.5
    ## 12          2011     25    1    -1.08   29  
    ## 13          2012     25    0    -2.56   24  
    ## 14          2013     17    4     0.647  21  
    ## 15          2014      9   -1    -4.33   19

``` r
ggplot(data=movies, aes(x=factor(thtr_rel_year), y=diff)) + geom_boxplot()
```

![](project_files/figure-gfm/deviations%20by%20demographics-3.png)<!-- -->

``` r
lm <- lm(data=movies, diff ~ thtr_rel_year)
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ thtr_rel_year, data = movies)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -37.83 -11.22   2.40  13.40  37.55 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)   -1393.2778   479.8268  -2.904  0.00395 **
    ## thtr_rel_year     0.6917     0.2391   2.893  0.00409 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.02 on 311 degrees of freedom
    ## Multiple R-squared:  0.0262, Adjusted R-squared:  0.02307 
    ## F-statistic: 8.368 on 1 and 311 DF,  p-value: 0.004087

``` r
month <- movies %>% group_by(thtr_rel_month) %>% summarise(movies = n_distinct(title), median = median(diff), mean = mean(diff), iqr = IQR(diff))
month
```

    ## # A tibble: 12 x 5
    ##    thtr_rel_month movies median   mean   iqr
    ##             <dbl>  <int>  <dbl>  <dbl> <dbl>
    ##  1              1     32   -7    -7.12  34  
    ##  2              2     13  -11   -13      9  
    ##  3              3     28   -7.5  -6.86  31.8
    ##  4              4     27   -5    -5.70  27  
    ##  5              5     19   -2    -5.95  19.5
    ##  6              6     33    2    -3.79  22  
    ##  7              7     21   -5    -5.62  38  
    ##  8              8     28   -1.5  -2.82  27.5
    ##  9              9     31   -1    -3.48  21  
    ## 10             10     35   -3    -5.29  24.5
    ## 11             11     22   -2.5  -4.45  22.8
    ## 12             12     23    1    -2.61   8.5

``` r
ggplot(data=movies, aes(x=factor(thtr_rel_month), y=diff)) + geom_boxplot()
```

![](project_files/figure-gfm/deviations%20by%20demographics-4.png)<!-- -->

``` r
lm <- lm(data=movies, diff ~ factor(thtr_rel_month))
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ factor(thtr_rel_month), data = movies)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -37.88 -11.52   2.00  13.29  36.12 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)               -7.1212     3.0277  -2.352   0.0193 *
    ## factor(thtr_rel_month)2   -5.8788     5.6954  -1.032   0.3028  
    ## factor(thtr_rel_month)3    0.2641     4.4689   0.059   0.9529  
    ## factor(thtr_rel_month)4    1.4175     4.5135   0.314   0.7537  
    ## factor(thtr_rel_month)5    1.1738     5.0089   0.234   0.8149  
    ## factor(thtr_rel_month)6    3.3333     4.2819   0.778   0.4369  
    ## factor(thtr_rel_month)7    1.5022     4.8552   0.309   0.7572  
    ## factor(thtr_rel_month)8    4.2998     4.4689   0.962   0.3367  
    ## factor(thtr_rel_month)9    3.6373     4.3504   0.836   0.4038  
    ## factor(thtr_rel_month)10   1.8355     4.2203   0.435   0.6639  
    ## factor(thtr_rel_month)11   2.6667     4.7873   0.557   0.5779  
    ## factor(thtr_rel_month)12   4.5125     4.7244   0.955   0.3403  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.39 on 301 degrees of freedom
    ## Multiple R-squared:  0.01613,    Adjusted R-squared:  -0.01982 
    ## F-statistic: 0.4487 on 11 and 301 DF,  p-value: 0.9327

``` r
day <- movies %>% group_by(thtr_rel_day) %>% summarise(movies = n_distinct(title), median = median(diff), mean = mean(diff), iqr = IQR(diff))
day
```

    ## # A tibble: 31 x 5
    ##    thtr_rel_day movies median   mean   iqr
    ##           <dbl>  <int>  <dbl>  <dbl> <dbl>
    ##  1            1     11   -1    -6     19  
    ##  2            2      7   -9    -9     27  
    ##  3            3      6   -7.5 -10.8   14.2
    ##  4            4      3   -9   -16.3   14  
    ##  5            5     13   -3    -4.54  19  
    ##  6            6     12    0    -3.42  23.8
    ##  7            7     12   -8.5  -8.75  15  
    ##  8            8      9   -3    -8.22  17  
    ##  9            9     11   -3    -6.91  33.5
    ## 10           10     14    0    -2.29  15.8
    ## # ... with 21 more rows

``` r
ggplot(data=movies, aes(x=factor(thtr_rel_day), y=diff)) + geom_boxplot()
```

![](project_files/figure-gfm/deviations%20by%20demographics-5.png)<!-- -->

``` r
lm <- lm(data=movies, diff ~ factor(thtr_rel_day))
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ factor(thtr_rel_day), data = movies)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -40.87 -12.46   2.00  12.33  34.91 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)             -6.0000     5.2505  -1.143    0.254
    ## factor(thtr_rel_day)2   -3.0000     8.4195  -0.356    0.722
    ## factor(thtr_rel_day)3   -4.8333     8.8378  -0.547    0.585
    ## factor(thtr_rel_day)4  -10.3333    11.3423  -0.911    0.363
    ## factor(thtr_rel_day)5    1.4615     7.1340   0.205    0.838
    ## factor(thtr_rel_day)6    2.5833     7.2689   0.355    0.723
    ## factor(thtr_rel_day)7   -2.7500     7.2689  -0.378    0.705
    ## factor(thtr_rel_day)8   -2.2222     7.8269  -0.284    0.777
    ## factor(thtr_rel_day)9   -0.9091     7.4253  -0.122    0.903
    ## factor(thtr_rel_day)10   3.7143     7.0162   0.529    0.597
    ## factor(thtr_rel_day)11   7.8667     6.9126   1.138    0.256
    ## factor(thtr_rel_day)12   3.0000     7.1340   0.421    0.674
    ## factor(thtr_rel_day)13   6.6000     7.6086   0.867    0.386
    ## factor(thtr_rel_day)14  -1.1250     8.0915  -0.139    0.890
    ## factor(thtr_rel_day)15  -1.2500     6.8206  -0.183    0.855
    ## factor(thtr_rel_day)16  -2.1000     7.6086  -0.276    0.783
    ## factor(thtr_rel_day)17   0.2500     7.2689   0.034    0.973
    ## factor(thtr_rel_day)18  -0.3333     7.8269  -0.043    0.966
    ## factor(thtr_rel_day)19  -5.5000     7.2689  -0.757    0.450
    ## factor(thtr_rel_day)20  -9.5455     7.4253  -1.286    0.200
    ## factor(thtr_rel_day)21   7.3077     7.1340   1.024    0.307
    ## factor(thtr_rel_day)22   0.3333     6.9126   0.048    0.962
    ## factor(thtr_rel_day)23  -1.8182     7.4253  -0.245    0.807
    ## factor(thtr_rel_day)24   6.0000    10.1675   0.590    0.556
    ## factor(thtr_rel_day)25   7.7273     7.4253   1.041    0.299
    ## factor(thtr_rel_day)26   9.0000     8.4195   1.069    0.286
    ## factor(thtr_rel_day)27   6.0000     7.6086   0.789    0.431
    ## factor(thtr_rel_day)28  -6.5000     8.0915  -0.803    0.422
    ## factor(thtr_rel_day)29   0.7500     8.0915   0.093    0.926
    ## factor(thtr_rel_day)30  -3.2500     8.0915  -0.402    0.688
    ## factor(thtr_rel_day)31  10.5000    10.1675   1.033    0.303
    ## 
    ## Residual standard error: 17.41 on 282 degrees of freedom
    ## Multiple R-squared:  0.07603,    Adjusted R-squared:  -0.02226 
    ## F-statistic: 0.7735 on 30 and 282 DF,  p-value: 0.7989

We again find some interesting trends from the above. These are, in no
particular order, as follows:

1.  Action and Adventure, Animation, and Comedy movies seem to have the
    largest negative deviations between critic and audience scores.
    Documentaries, sci-fi, and mystery movies have some of the smallest
    deviations. This might imply that some movies attract a different
    type of audience, some of which are slightly more critical than the
    average movie-goer.

2.  Unrated movies are the only movies in our dataset with a positive
    deviation, on average. All other ratings have similar negative
    deviations.

3.The month of a movie’s theatre release doesn’t seem to be a
significant predictor of a deviation between a critic and the audience
scores.

4.  The day of a theatre release doesn’t seem to be associated with the
    deviation between an audience and critics score.

5.  The difference in weeks between a theatre release date and dvd
    release date doesn’t seem to have significant predictive
power.r

-----

## Part 4: Modeling

``` r
lm1 <- lm(data=movies, diff ~  factor(genre) + factor(mpaa_rating) + log(runtime) + log(imdb_num_votes) + log(date_diff))
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ factor(genre) + factor(mpaa_rating) + log(runtime) + 
    ##     log(imdb_num_votes) + log(date_diff), data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -38.326 -12.245   0.747  12.652  40.261 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                            -69.9316    32.9081  -2.125   0.0344 *
    ## factor(genre)Animation                   2.3392     9.2089   0.254   0.7997  
    ## factor(genre)Art House & International   2.0496     6.9419   0.295   0.7680  
    ## factor(genre)Comedy                      2.8741     3.9760   0.723   0.4703  
    ## factor(genre)Documentary                12.1680     4.8555   2.506   0.0127 *
    ## factor(genre)Drama                       6.9845     3.4867   2.003   0.0461 *
    ## factor(genre)Horror                      8.0603     6.7200   1.199   0.2313  
    ## factor(genre)Musical & Performing Arts  18.5744     7.6645   2.423   0.0160 *
    ## factor(genre)Mystery & Suspense          6.4979     4.5185   1.438   0.1515  
    ## factor(genre)Other                       4.7720     9.1065   0.524   0.6007  
    ## factor(genre)Science Fiction & Fantasy  14.3392    10.0857   1.422   0.1562  
    ## factor(mpaa_rating)PG                   -0.9063    10.2190  -0.089   0.9294  
    ## factor(mpaa_rating)PG-13                -1.7270    10.5999  -0.163   0.8707  
    ## factor(mpaa_rating)R                     2.6600    10.5138   0.253   0.8004  
    ## factor(mpaa_rating)Unrated               7.6621    10.6163   0.722   0.4710  
    ## log(runtime)                            12.1581     7.4076   1.641   0.1018  
    ## log(imdb_num_votes)                     -0.1899     0.7642  -0.249   0.8039  
    ## log(date_diff)                           0.7157     1.2870   0.556   0.5786  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.55 on 295 degrees of freedom
    ## Multiple R-squared:  0.1273, Adjusted R-squared:  0.07697 
    ## F-statistic: 2.531 on 17 and 295 DF,  p-value: 0.0008852

``` r
lm1 <- lm(data=movies, diff ~  factor(genre) + factor(mpaa_rating) + log(runtime) + log(date_diff))
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ factor(genre) + factor(mpaa_rating) + log(runtime) + 
    ##     log(date_diff), data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -38.433 -12.126   0.648  12.781  40.457 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)                            -68.5014    32.3495  -2.118  0.03505 * 
    ## factor(genre)Animation                   2.2437     9.1863   0.244  0.80721   
    ## factor(genre)Art House & International   2.2563     6.8810   0.328  0.74321   
    ## factor(genre)Comedy                      2.9783     3.9475   0.754  0.45116   
    ## factor(genre)Documentary                12.5472     4.6022   2.726  0.00679 **
    ## factor(genre)Drama                       7.1215     3.4375   2.072  0.03916 * 
    ## factor(genre)Horror                      8.0682     6.7092   1.203  0.23011   
    ## factor(genre)Musical & Performing Arts  18.8423     7.5763   2.487  0.01343 * 
    ## factor(genre)Mystery & Suspense          6.6022     4.4919   1.470  0.14268   
    ## factor(genre)Other                       4.7933     9.0916   0.527  0.59843   
    ## factor(genre)Science Fiction & Fantasy  14.2290    10.0600   1.414  0.15829   
    ## factor(mpaa_rating)PG                   -1.1278    10.1639  -0.111  0.91172   
    ## factor(mpaa_rating)PG-13                -2.0000    10.5261  -0.190  0.84944   
    ## factor(mpaa_rating)R                     2.3892    10.4406   0.229  0.81915   
    ## factor(mpaa_rating)Unrated               7.6843    10.5991   0.725  0.46903   
    ## log(runtime)                            11.4182     6.7721   1.686  0.09284 . 
    ## log(date_diff)                           0.7732     1.2640   0.612  0.54119   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.52 on 296 degrees of freedom
    ## Multiple R-squared:  0.1271, Adjusted R-squared:  0.0799 
    ## F-statistic: 2.693 on 16 and 296 DF,  p-value: 0.0005246

``` r
lm1 <- lm(data=movies, diff ~  factor(genre) + factor(mpaa_rating) + log(runtime))
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ factor(genre) + factor(mpaa_rating) + log(runtime), 
    ##     data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -38.845 -11.838   0.905  12.847  40.662 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)                            -65.4786    31.9362  -2.050  0.04121 * 
    ## factor(genre)Animation                   2.0305     9.1700   0.221  0.82491   
    ## factor(genre)Art House & International   2.7440     6.8274   0.402  0.68804   
    ## factor(genre)Comedy                      2.8446     3.9373   0.722  0.47057   
    ## factor(genre)Documentary                12.7728     4.5826   2.787  0.00566 **
    ## factor(genre)Drama                       7.1680     3.4330   2.088  0.03765 * 
    ## factor(genre)Horror                      8.0691     6.7022   1.204  0.22956   
    ## factor(genre)Musical & Performing Arts  19.2683     7.5363   2.557  0.01106 * 
    ## factor(genre)Mystery & Suspense          6.5857     4.4871   1.468  0.14324   
    ## factor(genre)Other                       4.6259     9.0779   0.510  0.61072   
    ## factor(genre)Science Fiction & Fantasy  14.1328    10.0482   1.407  0.16062   
    ## factor(mpaa_rating)PG                   -0.9809    10.1504  -0.097  0.92308   
    ## factor(mpaa_rating)PG-13                -1.9187    10.5142  -0.182  0.85533   
    ## factor(mpaa_rating)R                     2.6205    10.4227   0.251  0.80166   
    ## factor(mpaa_rating)Unrated               7.8322    10.5852   0.740  0.45993   
    ## log(runtime)                            11.2399     6.7587   1.663  0.09736 . 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.5 on 297 degrees of freedom
    ## Multiple R-squared:  0.126,  Adjusted R-squared:  0.08184 
    ## F-statistic: 2.854 on 15 and 297 DF,  p-value: 0.0003367

``` r
lm1 <- lm(data=movies, diff ~  factor(genre) + log(runtime))
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ factor(genre) + log(runtime), data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -38.781 -12.345   1.613  11.651  43.335 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                             -60.667     30.909  -1.963  0.05060 .  
    ## factor(genre)Animation                    1.686      6.982   0.242  0.80928    
    ## factor(genre)Art House & International    7.614      6.589   1.155  0.24883    
    ## factor(genre)Comedy                       2.959      3.934   0.752  0.45256    
    ## factor(genre)Documentary                 17.839      4.017   4.441 1.26e-05 ***
    ## factor(genre)Drama                        8.852      3.369   2.627  0.00905 ** 
    ## factor(genre)Horror                      11.485      6.591   1.742  0.08245 .  
    ## factor(genre)Musical & Performing Arts   22.723      7.442   3.053  0.00247 ** 
    ## factor(genre)Mystery & Suspense           7.968      4.410   1.807  0.07178 .  
    ## factor(genre)Other                        9.012      8.993   1.002  0.31707    
    ## factor(genre)Science Fiction & Fantasy   14.407     10.108   1.425  0.15510    
    ## log(runtime)                             10.151      6.710   1.513  0.13140    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.61 on 301 degrees of freedom
    ## Multiple R-squared:  0.1027, Adjusted R-squared:  0.06986 
    ## F-statistic:  3.13 on 11 and 301 DF,  p-value: 0.0005099

``` r
lm1 <- lm(data=movies, diff ~  factor(genre) + factor(mpaa_rating) + log(runtime))
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = diff ~ factor(genre) + factor(mpaa_rating) + log(runtime), 
    ##     data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -38.845 -11.838   0.905  12.847  40.662 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)                            -65.4786    31.9362  -2.050  0.04121 * 
    ## factor(genre)Animation                   2.0305     9.1700   0.221  0.82491   
    ## factor(genre)Art House & International   2.7440     6.8274   0.402  0.68804   
    ## factor(genre)Comedy                      2.8446     3.9373   0.722  0.47057   
    ## factor(genre)Documentary                12.7728     4.5826   2.787  0.00566 **
    ## factor(genre)Drama                       7.1680     3.4330   2.088  0.03765 * 
    ## factor(genre)Horror                      8.0691     6.7022   1.204  0.22956   
    ## factor(genre)Musical & Performing Arts  19.2683     7.5363   2.557  0.01106 * 
    ## factor(genre)Mystery & Suspense          6.5857     4.4871   1.468  0.14324   
    ## factor(genre)Other                       4.6259     9.0779   0.510  0.61072   
    ## factor(genre)Science Fiction & Fantasy  14.1328    10.0482   1.407  0.16062   
    ## factor(mpaa_rating)PG                   -0.9809    10.1504  -0.097  0.92308   
    ## factor(mpaa_rating)PG-13                -1.9187    10.5142  -0.182  0.85533   
    ## factor(mpaa_rating)R                     2.6205    10.4227   0.251  0.80166   
    ## factor(mpaa_rating)Unrated               7.8322    10.5852   0.740  0.45993   
    ## log(runtime)                            11.2399     6.7587   1.663  0.09736 . 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.5 on 297 degrees of freedom
    ## Multiple R-squared:  0.126,  Adjusted R-squared:  0.08184 
    ## F-statistic: 2.854 on 15 and 297 DF,  p-value: 0.0003367

Above, we fitted a model using R^2 Backward Elimination. I started with
a full-model including all of the variables that were shown to be
significant predictors above. I then removed the model with the smallest
p-value in each around and watched how the adjusted R^2 value changed
each time I did that. Adjusted R^2 increased for every variable I
removed until I attempted to remove MPAA Rating near the end of my model
selection. When I did this, my adjusted R^2 actually decreased from the
prior model, so I folded it back in.

Now that we have a model, lets run some diagnostics on it. We want to
check for: a. linear relationship between x and y b. nearly normal
residuals c. constant variability of residuals d. independence of
residuals

First, lets check for a linear relationship between X and Y.

``` r
plot(lm1$residuals ~ log(movies$runtime))
```

![](project_files/figure-gfm/linear%20relationship-1.png)<!-- -->

Looks like our residuals are randomly scattered around zero. We have met
this condition.

Now lets look at the condition of nearly normal residuals. To do this,
we will plot a histogram of the residuals and check for normality and a
mean around zero.

``` r
hist(lm1$residuals)
```

![](project_files/figure-gfm/normal%20residuals-1.png)<!-- -->

It looks like we have clearly satisfied the condition of nearly normal
residuals, as the distribution is centered at zero and is distributed
normally around it.

Now, lets check for the constant variability of residuals. We will do
this by plotting the residuals with the fitted and check for random
scatter here.

``` r
plot(abs(lm1$residuals) ~ abs(lm1$fitted))
```

![](project_files/figure-gfm/constant%20variability-1.png)<!-- -->

Again, it looks this like condition is satisfied\!

Lets move onto the last condition…independence of residuals.

``` r
plot(lm1$residuals)
```

![](project_files/figure-gfm/ind%20of%20residuals-1.png)<!-- -->

These are randomly scattered. Thus, all assumptions are satisfied and
our model is valid\!

-----

## Part 5: Prediction

We are going to see what values we predict for the 2016 movie ‘Arrival’.
This was a movie that seemed to receive a lot of attention from critics,
but less attention and praise from the general population.

``` r
model <- movies %>% select(title, genre, mpaa_rating, runtime)
model <- add_row(model, title = 'Arrival', genre = 'Science Fiction & Fantasy', mpaa_rating = 'PG-13', runtime = 118)
arrival <- subset(model, title == 'Arrival')

predict(lm1, newdata = arrival, interval = "confidence")
```

    ##         fit       lwr      upr
    ## 1 0.3575855 -18.66502 19.38019

From the above, we can see that we are predicting a difference of nearly
zero with a confidence interval of between -18.6 and +19.3. This means
we are 95% confident that, on average and with all else held equal, the
difference between the critic score and audience scores for the movie
Arrival lies between -18.6 and 19.3.

-----

## Part 6: Conclusion

In conclusion, we have built a significant model for predicting the
difference in the critic score and audience score for any movie made
after 2000. However, the adjusted R^2 implies that we can only explain
about 8% of the variability in the difference between an audience and
critic score for a movie.

This implies that it is likely to be the case that there are other
variables we need to bring into our model to better predict this model.
Unfortunately, we are only given a small set of variables for this
project.

Regardless, we discovered some interesting explanatory variables that we
will be able to report back to our boss at Paramount Pictures.
