library(dplyr)
library(ggplot2)
library(DescTools)
library(tree)


train <- read.csv("/Users/hanson377/Documents/data-projects/works in progress/House Prices/train.csv")
test <- read.csv("/Users/hanson377/Documents/data-projects/works in progress/House Prices/test.csv")

## we are going to see if we can predict the neighborhood a tree is in.
