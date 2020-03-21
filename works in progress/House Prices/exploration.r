train <- read.csv("C:/Users/hanso/Desktop/Kaggle/House Prices/train.csv")
test <- read.csv("C:/Users/hanso/Desktop/Kaggle/House Prices/test.csv")

library(dplyr)
library(ggplot2)
library(DescTools)


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
