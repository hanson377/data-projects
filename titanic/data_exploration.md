Quick Exploration of Classic Titanic Dataset
================

## Load packages and data

Simply loading the packages we will need for this exercise as well as
the data we will need.

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
library(DescTools)
```

    ## Warning: package 'DescTools' was built under R version 3.6.2

``` r
library(InformationValue)
```

    ## Warning: package 'InformationValue' was built under R version 3.6.2

``` r
data <- read.csv("C:/Users/hanso/OneDrive/Documents/GitHub/data-projects/titanic/train.csv")
```

## Exploratory data analysis

Lets begin by looking at survival rates for some variables that I think
might show us something
interesting.

``` r
data %>% group_by(Pclass) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 3 x 3
    ##   Pclass  mean sample
    ##    <int> <dbl>  <int>
    ## 1      1 0.630    216
    ## 2      2 0.473    184
    ## 3      3 0.242    491

``` r
data %>% group_by(Sex) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 2 x 3
    ##   Sex     mean sample
    ##   <fct>  <dbl>  <int>
    ## 1 female 0.742    314
    ## 2 male   0.189    577

``` r
data %>% group_by(Embarked) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 4 x 3
    ##   Embarked  mean sample
    ##   <fct>    <dbl>  <int>
    ## 1 ""       1          2
    ## 2 C        0.554    168
    ## 3 Q        0.390     77
    ## 4 S        0.337    644

``` r
data %>% group_by(Parch) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 7 x 3
    ##   Parch  mean sample
    ##   <int> <dbl>  <int>
    ## 1     0 0.344    678
    ## 2     1 0.551    118
    ## 3     2 0.5       80
    ## 4     3 0.6        5
    ## 5     4 0          4
    ## 6     5 0.2        5
    ## 7     6 0          1

``` r
data %>% group_by(Parch) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 7 x 3
    ##   Parch  mean sample
    ##   <int> <dbl>  <int>
    ## 1     0 0.344    678
    ## 2     1 0.551    118
    ## 3     2 0.5       80
    ## 4     3 0.6        5
    ## 5     4 0          4
    ## 6     5 0.2        5
    ## 7     6 0          1

``` r
data %>% group_by(SibSp) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 7 x 3
    ##   SibSp  mean sample
    ##   <int> <dbl>  <int>
    ## 1     0 0.345    608
    ## 2     1 0.536    209
    ## 3     2 0.464     28
    ## 4     3 0.25      16
    ## 5     4 0.167     18
    ## 6     5 0          5
    ## 7     8 0          7

``` r
data <- data %>% mutate(
    parch2 = case_when(Parch == 0 ~ 0,
                      Parch > 0 ~ 1))
  data <- data %>% mutate(
      sib2 = case_when(SibSp == 0 ~ 0,
                        SibSp > 0 ~ 1))
data <- data %>% mutate(embark_alt = case_when(Embarked == "C" ~ "C", Embarked == "Q" ~ "Other", Embarked == "S" ~ "Other"))

data %>% group_by(parch2) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 2 x 3
    ##   parch2  mean sample
    ##    <dbl> <dbl>  <int>
    ## 1      0 0.344    678
    ## 2      1 0.512    213

``` r
data %>% group_by(sib2) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 2 x 3
    ##    sib2  mean sample
    ##   <dbl> <dbl>  <int>
    ## 1     0 0.345    608
    ## 2     1 0.466    283

``` r
data %>% group_by(embark_alt) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 3 x 3
    ##   embark_alt  mean sample
    ##   <chr>      <dbl>  <int>
    ## 1 <NA>       1          2
    ## 2 C          0.554    168
    ## 3 Other      0.343    721

## A few quick observations

All of the above seem to vary greatly by different levels.  
We will want to include these in our modeling efforts.

Now, lets look at the relationship between survival rates and some
continuous
variables

``` r
data %>% group_by(Survived) %>% summarise(mean = mean(Fare), median(Fare), iqr = IQR(Fare))
```

    ## # A tibble: 2 x 4
    ##   Survived  mean `median(Fare)`   iqr
    ##      <int> <dbl>          <dbl> <dbl>
    ## 1        0  22.1           10.5  18.1
    ## 2        1  48.4           26    44.5

``` r
ggplot(data=data, aes(x=factor(Survived), y=Fare)) + geom_boxplot()
```

![](data_exploration_files/figure-gfm/survival%20by%20continuous%20variables-1.png)<!-- -->

``` r
ggplot(data=data, aes(x=factor(Survived), y=Age)) + geom_boxplot()
```

    ## Warning: Removed 177 rows containing non-finite values (stat_boxplot).

![](data_exploration_files/figure-gfm/survival%20by%20continuous%20variables-2.png)<!-- -->

``` r
ggplot(data=data, aes(x=Fare, fill=factor(Sex))) + geom_density()
```

![](data_exploration_files/figure-gfm/survival%20by%20continuous%20variables-3.png)<!-- -->

``` r
ggplot(data=data, aes(x=Fare, fill=factor(Survived))) + geom_density() + facet_wrap(~ factor(Sex))
```

![](data_exploration_files/figure-gfm/survival%20by%20continuous%20variables-4.png)<!-- -->

## A quick observation

From the above, it seems pretty clear your odds of survival were
depending on your fare
amount.

``` r
data %>% group_by(Survived) %>% summarise(mean = mean(Age, na.rm=TRUE), median(Age,na.rm=TRUE), iqr = IQR(Age,na.rm=TRUE))
```

    ## # A tibble: 2 x 4
    ##   Survived  mean `median(Age, na.rm = TRUE)`   iqr
    ##      <int> <dbl>                       <dbl> <dbl>
    ## 1        0  30.6                          28    18
    ## 2        1  28.3                          28    17

``` r
ggplot(data=data, aes(x=factor(Survived), y=Age)) + geom_boxplot()
```

    ## Warning: Removed 177 rows containing non-finite values (stat_boxplot).

![](data_exploration_files/figure-gfm/more%20analysis-1.png)<!-- -->

``` r
ggplot(data=data, aes(x=Fare, fill=factor(Embarked))) + geom_density()
```

![](data_exploration_files/figure-gfm/more%20analysis-2.png)<!-- -->

``` r
ggplot(data=data, aes(x=Fare, colour=factor(Embarked))) + stat_ecdf()
```

![](data_exploration_files/figure-gfm/more%20analysis-3.png)<!-- -->

``` r
ggplot(data = data, aes(x=Fare)) + geom_histogram() + facet_wrap(~Survived)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](data_exploration_files/figure-gfm/more%20analysis-4.png)<!-- -->

``` r
ggplot(data = data, aes(x=Fare, fill=sib2)) + geom_density() + facet_wrap(~Survived)
```

![](data_exploration_files/figure-gfm/more%20analysis-5.png)<!-- -->

Finally, lets look at the cabin field. seems we have a lot of nulls.
lets fill those in with “unknown”

``` r
data <- data %>% mutate(cabin_alt = case_when(
        Cabin == "" ~ 'no value',
        Cabin %like% "A" ~ "A",
        Cabin %like% "B" ~ "B",
        Cabin %like% "C" ~ "C",
        Cabin %like% "D" ~ "D",
        Cabin %like% "E" ~ "E",
        Cabin %like% "F" ~ "F",
        Cabin %like% "G" ~ "G",
        Cabin %like% "T" ~ "T"
))
data %>% group_by(cabin_alt) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 4 x 3
    ##   cabin_alt  mean sample
    ##   <chr>     <dbl>  <int>
    ## 1 <NA>      0.67     200
    ## 2 D         0.667      3
    ## 3 no value  0.300    687
    ## 4 T         0          1

``` r
data <- data %>% mutate(cabin2 = case_when (
Cabin == "" ~ "no value OR A/G/F",
Cabin %like% "A" ~ "no value OR A/G/F",
Cabin %like% "G" ~ "no value OR A/G/F",
Cabin %like% "F" ~ "no value OR A/G/F",
Cabin %like% "B" ~ "Other",
Cabin %like% "C" ~ "Other",
Cabin %like% "D" ~ "Other",
Cabin %like% "E" ~ "Other",
Cabin %like% "T" ~ "Other"
))
data %>% group_by(cabin2) %>% summarise(mean = mean(Survived), sample = n())
```

    ## # A tibble: 3 x 3
    ##   cabin2             mean sample
    ##   <chr>             <dbl>  <int>
    ## 1 <NA>              0.67     200
    ## 2 no value OR A/G/F 0.300    687
    ## 3 Other             0.5        4

Looks like we have something here.  
People with a designated cabin seem to have much higher survival rates.

## Model Building

Now, let us build a very simple model with what we learned from above.
We will begin by creating some training and test data. Below, we create
the training and test data.

``` r
input_ones <- data[which(data$Survived == 1), ]  # all 1's
input_zeros <- data[which(data$Survived == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's

test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's
```

Now that we have some data, lets begin building our model using our
findings from above and condition it with the training
data.

``` r
fit1 <- glm(Survived ~ factor(Pclass) + factor(embark_alt) + factor(Sex) + factor(cabin2) + Fare, data = trainingData, family = "binomial")
summary(fit1)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ factor(Pclass) + factor(embark_alt) + 
    ##     factor(Sex) + factor(cabin2) + Fare, family = "binomial", 
    ##     data = trainingData)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8863  -0.6597  -0.5675   0.7661   1.9567  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              2.093884   0.804850   2.602  0.00928 ** 
    ## factor(Pclass)2         -0.195727   0.695828  -0.281  0.77849    
    ## factor(Pclass)3         -1.043143   0.689181  -1.514  0.13013    
    ## factor(embark_alt)Other -0.331095   0.377218  -0.878  0.38009    
    ## factor(Sex)male         -2.474435   0.267243  -9.259  < 2e-16 ***
    ## factor(cabin2)Other     -0.132323   1.460148  -0.091  0.92779    
    ## Fare                     0.001295   0.006545   0.198  0.84311    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 484.03  on 355  degrees of freedom
    ## Residual deviance: 358.71  on 349  degrees of freedom
    ##   (122 observations deleted due to missingness)
    ## AIC: 372.71
    ## 
    ## Number of Fisher Scoring iterations: 4
