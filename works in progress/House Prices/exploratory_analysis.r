library(ggplot2)

tr <- read.csv("C:/Users/hanso/Desktop/Kaggle/House Prices/train.csv", header=TRUE)
View(train)


p <- ggplot(tr, aes(Neighborhood, SalePrice))
p + geom_boxplot()
##neighborhood certainly seems to have price differences
## lets test differences

a1 <- aov(SalePrice ~ factor(Neighborhood), data=tr)
summary(a1)


p <- ggplot(tr, aes(YrSold, SalePrice,group = YrSold))
p + geom_boxplot()
##nothing great here

p <- ggplot(tr, aes(YearBuilt, SalePrice,group = YearBuilt))
p + geom_boxplot()
## interesting trend here. hockey stick curve with 1980 being the breaking point


p <- ggplot(tr, aes(SaleCondition, SalePrice))
p + geom_boxplot()
## partial seems to have the highest median, followed by normal but nortmal has more outliers


# Basic Scatterplot Matrix
pairs(~SalePrice+YearBuilt+LotArea+OverallQual+OverallCond+YearRemodAdd,data=tr,
   main="Simple Scatterplot Matrix")

## YearRemodAdd, OverallCond have interesting trends

res.aov <- aov(SalePrice ~ factor(OverallCond), data = tr)
summary(res.aov)
TukeyHSD(res.aov)

## five and four here seem to have significantly different values than the rest

p <- ggplot(tr, aes(OverallCond, SalePrice,group = OverallCond))
p + geom_boxplot()

## 5 and 9 are similar, everything below 5 is sig diff, everything between 6 and 8 are sig diff
## create factor to account for this

tr$condition <- 'error'
tr$condition[tr$OverallCond == 5 | tr$OverallCond == 9] <- '5 or 9'
tr$condition[tr$OverallCond < 5 ] <- 'less than 5'
tr$condition[tr$OverallCond > 5 & tr$OverallCond < 9 ] <- 'between 5 and 9'

aov <- aov(SalePrice ~ factor(condition), data=tr)
summary(aov)
TukeyHSD(aov)
