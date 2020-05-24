library(dplyr)
library(ggplot2)
library(DescTools)


train <- read.csv("C:/Users/hanso/OneDrive/Documents/GitHub/data-projects/works in progress/House Prices/train.csv")
test <- read.csv("C:/Users/hanso/OneDrive/Documents/GitHub/data-projects/works in progress/House Prices/test.csv")

## First, lets simply do some EDA.
## Our variable we are attempting to predict is 'SalePrice'.  Lets get an idea of the range here.

ggplot(train, aes(x=SalePrice)) + geom_density()

## as we might have expected, there is a clean median but a pretty long right tail.
## lets look at how this histogram differs by various categorical variables.
ggplot(train, aes(x=SalePrice,fill=Neighborhood)) + geom_density(alpha=.2)
ggplot(train, aes(x=SalePrice,fill=YearBuilt)) + geom_density(alpha=.2)


##extract only numeric variables to build correlation matrix

numeric <- select_if(train, is.numeric)
res <- cor(numeric)
round(res, 2)
res

library(Hmisc)
res2<-rcorr(as.matrix(numeric))
res2<-flattenCorrMatrix(res2$r, res2$P)
res2$p_value <- round(res2$p*100, digits=2)
res2 <- subset(res2, column == "SalePrice")

## we now have a list of the most strongly correlated variables with SalePrice!
## lets now identify the factor variables sig diff from eachother
