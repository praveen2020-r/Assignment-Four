# Get and print current working directory.
rm()
packages = c("tidyverse", "RCurl", "psych", "stats", 
             "randomForest", "glmnet", "caret","kernlab", 
             "rpart", "rpart.plot", "neuralnet", "C50",
             "doParallel", "AUC", "ggfortify")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
invisible(lapply(packages, require, character.only = TRUE))

data <- read.csv("winequality-white.csv",header = T, sep = ";")
n = nrow(data); p = ncol(data); dim(data)

any(is.na(data))

library(psych)
summary(data)

colnames(data)


set.seed(1)
idx = sample(n, 0.9*n)
train = data[idx,]; dim(train)
test = data[-idx,]; dim(test)

normalize_train = function(x) (x - min(x))/(max(x) - min(x))
train.norm = data.frame(apply(train[,-p], 2, normalize_train), 
                        quality = train[,p])
summary(train.norm)
# normalize test set using the values from train set to make prediction comparable
train.min = apply(train[,-p], 2, min)
train.max = apply(train[,-p], 2, max)
test.norm = data.frame(sweep(test, 2, c(train.min, 0)) %>% 
                         sweep(2, c(train.max-train.min, 1), FUN = "/"))
summary(test.norm) # test.norm might have data out of range 0~1, since it's normalized against the training set.

hist(data$quality)

shapiro.test(data$quality) #Didn't pass normality test, so linear model may have a problem
tr.lm = lm(quality~., data = train.norm)
summary(tr.lm)

