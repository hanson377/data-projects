data <- read.csv("C:/Users/hanso/Desktop/Kaggle/titanic/train.csv")


library(dplyr)
library(ggplot2)
library(DescTools)

## first, lets check survival rates by some of our categorical variables

data %>% group_by(Pclass) %>% summarise(mean = mean(Survived), sample = n())
data %>% group_by(Sex) %>% summarise(mean = mean(Survived), sample = n())
data %>% group_by(Embarked) %>% summarise(mean = mean(Survived), sample = n())
data %>% group_by(Parch) %>% summarise(mean = mean(Survived), sample = n())

## all of the above seem to vary greatly by different levels.
data %>% group_by(Parch) %>% summarise(mean = mean(Survived), sample = n())
data %>% group_by(SibSp) %>% summarise(mean = mean(Survived), sample = n())

data <- data %>% mutate(
    parch2 = case_when(Parch == 0 ~ 0,
                      Parch > 0 ~ 1))
  data <- data %>% mutate(
      sib2 = case_when(SibSp == 0 ~ 0,
                        SibSp > 0 ~ 1))
data <- data %>% mutate(embark_alt = case_when(Embarked == "C" ~ "C", Embarked == "Q" ~ "Other", Embarked == "S" ~ "Other"))

data %>% group_by(parch2) %>% summarise(mean = mean(Survived), sample = n())
data %>% group_by(sib2) %>% summarise(mean = mean(Survived), sample = n())
data %>% group_by(embark_alt) %>% summarise(mean = mean(Survived), sample = n())

##

## now, lets look at the relationship between survival and some continuous variables

data %>% group_by(Survived) %>% summarise(mean = mean(Fare), median(Fare), iqr = IQR(Fare))
ggplot(data=data, aes(x=factor(Survived), y=Fare)) + geom_boxplot()
ggplot(data=data, aes(x=factor(Survived), y=Age)) + geom_boxplot()

ggplot(data=data, aes(x=Fare, fill=factor(Sex))) + geom_density()
ggplot(data=data, aes(x=Fare, fill=factor(Survived))) + geom_density() + facet_wrap(~ factor(Sex))


## seems pretty clear your odds of survival were depending on your fare.

data %>% group_by(Survived) %>% summarise(mean = mean(Age), median(Age), iqr = IQR(Age))
ggplot(data=data, aes(x=factor(Survived), y=Age)) + geom_boxplot()
ggplot(data=data, aes(x=Fare, fill=factor(Embarked))) + geom_density()
ggplot(data=data, aes(x=Fare, colour=factor(Embarked))) + stat_ecdf()

ggplot(data = data, aes(x=Fare)) + geom_histogram() + facet_wrap(~Survived)
ggplot(data = data, aes(x=Fare, fill=sib2)) + geom_density() + facet_wrap(~Survived)

## finally, lets look at the cabin. seems we have a lot of nulls. lets fill those in with "unknown"
data <- data %>% mutate(cabin_alt = case_when(
        Cabin == "" ~ 'no value',
        Cabin %ilike% "A" ~ "A",
        Cabin %ilike% "B" ~ "B",
        Cabin %ilike% "C" ~ "C",
        Cabin %ilike% "D" ~ "D",
        Cabin %ilike% "E" ~ "E",
        Cabin %ilike% "F" ~ "F",
        Cabin %ilike% "G" ~ "G",
        Cabin %ilike% "T" ~ "T"
))
data %>% group_by(cabin_alt) %>% summarise(mean = mean(Survived), sample = n())

## looks like we have something here. people with a designated cabin seem to have much higher survival rates.
data <- data %>% mutate(cabin2 = case_when (
Cabin == "" ~ "no value OR A/G/F",
Cabin %like% "A" ~ "no value OR A/G/F",
Cabin %like% "G" ~ "no value OR A/G/F",
Cabin %like% "F" ~ "no value OR A/G/F",
Cabin %ilike% "B" ~ "Other",
Cabin %ilike% "C" ~ "Other",
Cabin %ilike% "D" ~ "Other",
Cabin %ilike% "E" ~ "Other",
Cabin %ilike% "T" ~ "Other"
))
data %>% group_by(cabin2) %>% summarise(mean = mean(Survived), sample = n())



## clean and prepare training vs test data
# Create Training Data
input_ones <- data[which(data$Survived == 1), ]  # all 1's
input_zeros <- data[which(data$Survived == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's

## build the logit model
fit1 <- glm(Survived ~ factor(Pclass) + factor(embark_alt) + factor(Sex) + factor(cabin2) + Fare, data = trainingData, family = "binomial")
summary(fit1)


predicted <- plogis(predict(fit1, testData))  # predicted scores
optCutOff <- optimalCutoff(testData$Survived, predicted)[1]

misClassError(testData$Survived, predicted, threshold = optCutOff)
plotROC(testData$Survived, predicted)

Concordance(testData$Survived, predicted)
confusionMatrix(testData$Survived, predicted, threshold = optCutOff)
