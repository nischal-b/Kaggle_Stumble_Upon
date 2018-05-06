# loading libraries
library(readr)
library(Hmisc)
library(gmodels)
library(rpart)
library(rpart.plot)
library(mice)

# loading sources
source("Documents/4th_Sem/Biz_Intel/BabsonAnalytics.R")

# loading data
SU <- read_csv("Documents/4th_Sem/Biz_Intel/stumbleupon (2).csv")

SU1 = SU
#describe(SU1)

# Managing data
# Considering only those data entries whose compression_ratio lies in between 0 and 1
SU1 = sqldf("SELECT * FROM SU1 WHERE SU1.compression_ratio >= 0 AND SU1.compression_ratio <= 1") 

# Considering only those data entries whose embed_ratio lies in between 0 and 1
SU1 = sqldf("SELECT * FROM SU1 WHERE SU1.embed_ratio >= 0 AND SU1.embed_ratio <= 1") 

# Considering only those data entries whose image_ratio lies in between 0 and 1
SU1 = sqldf("SELECT * FROM SU1 WHERE SU1.image_ratio >= 0 AND SU1.image_ratio <= 1") 

# Considering only those data entries whose avglinksize is less than or equal to 5.538
SU1 = sqldf("SELECT * FROM SU1 WHERE SU1.avglinksize <= 5.538") 

# Removing the column framebased, as it has all 0 entries
SU1$framebased = NULL

# Coverting the hasDomainLink into logical (0,1)
SU1$hasDomainLink = as.logical(SU1$hasDomainLink)

# Coverting the label into logical (0,1)
SU1$label = as.logical(SU1$label)

# Coverting the lengthyLinkDomain into logical (0,1)
SU1$lengthyLinkDomain = as.logical(SU1$lengthyLinkDomain)

# Coverting the alchemy_category_score into numeric -> as a result NA s get created in the place of ?s
SU1$alchemy_category_score = as.numeric(SU1$alchemy_category_score)

# Coverting the alchemy_category into a categorical variable
SU1$alchemy_category = as.factor(SU1$alchemy_category)

# Coverting the news_front_page into a categorical variable
SU1$news_front_page = as.factor(SU1$news_front_page)

# Coverting is_news into a categorical variable
SU1$is_news = as.factor(SU1$is_news)

#
# Removing "?s"
SU1$alchemy_category[SU1$alchemy_category == "?"] <- NA
#SU1$is_news[(SU1$is_news == 1)] <- NA
SU1$is_news = (SU1$is_news == 1)
SU1$news_front_page[SU1$news_front_page == "?"] <- NA

# Imputation using cart (classification and regression trees)
impute_cart = mice(SU1, m = 5, method = "cart")
SU1 = complete(impute_cart)

# Removing url and bolier plate, as logistic regression or trees cannot work on strings
SU1$url = NULL
SU1$boilerplate = NULL

# glm (logistic regression) begin

# manage
SU1$label = as.logical(SU1$label)

df = SU1

# slice
N = nrow(SU1)

trainingSize = round(N * 0.6)
trainingCases = sample(N,trainingSize)
training = SU1[trainingCases,]
test = SU1[-trainingCases,]

# build

model = glm(label ~ ., data = training, family = binomial)
summary(model)
model = step(model)

pred = predict(model, test, type="response")
ROCChart(test$label, pred)
liftChart(test$label, pred)
predTF = (pred > 0.5)

# predictions
error = sum(predTF!=test$label)/nrow(test)
error_bench = benchmarkErrorRate(training$label,test$label)

# glm (logistic regression) end

# Classification Trees Begin

# manage
SU1$label = as.factor(SU1$label)

df = SU1

# slice
N = nrow(SU1)

trainingSize = round(N * 0.6)
trainingCases = sample(N,trainingSize)
training = SU1[trainingCases,]
test = SU1[-trainingCases,]

# pruning
stoppingRules = rpart.control(minsplit = 2, minbucket = 1, cp = 0)
overfit = rpart(label~., data = training, control = stoppingRules)
pruned = easyPrune(overfit)
rpart.plot(pruned)

# predictions
pred = predict(pruned, test, type = "class")
error = sum(pred!=test$label)/nrow(test)
error_bench = benchmarkErrorRate(training$label,test$label)

# Classification Trees End