## Load libraries and data
library(ggplot2)
library(caret)
library(corrplot)
setwd("C:/Dropbox/Education/Rwd/PML project")
raw.data <- read.csv("pml-training.csv")
cleansed.data <- raw.data

## Remove unnecessary variables
max.col.id <- grep("max_", colnames(cleansed.data))
cleansed.data <- cleansed.data[, -max.col.id]
min.col.id <- grep("min_", colnames(cleansed.data))
cleansed.data <- cleansed.data[, -min.col.id]
stddev.col.id <- grep("stddev_", colnames(cleansed.data))
cleansed.data <- cleansed.data[, -stddev.col.id]
var.col.id <- grep("var_", colnames(cleansed.data))
cleansed.data <- cleansed.data[, -var.col.id]
kurtosis.col.id <- grep("kurtosis_", colnames(cleansed.data))
cleansed.data <- cleansed.data[, -kurtosis.col.id]
skewness.col.id <- grep("skewness_", colnames(cleansed.data))
cleansed.data <- cleansed.data[, -skewness.col.id]
amplitude.col.id <- grep("amplitude_", colnames(cleansed.data))
cleansed.data <- cleansed.data[, -amplitude.col.id]
avg.col.id <- grep("avg_", colnames(cleansed.data))
cleansed.data <- cleansed.data[, -avg.col.id]

## Group by sensor
belt.col.id <- grep("_belt", colnames(cleansed.data))
arm.col.id <- grep("_arm", colnames(cleansed.data))
dumbbell.col.id <- grep("_dumbbell", colnames(cleansed.data))
forearm.col.id <- grep("_forearm", colnames(cleansed.data))

# length(arm.col.id) + length(belt.col.id) + length(dumbbell.col.id) + length(forearm.col.id)
# colnames(cleansed.data[ ,-c(arm.col.id, belt.col.id, dumbbell.col.id, forearm.col.id)])
# colnames(cleansed.data[ ,c(arm.col.id)])
# colnames(cleansed.data[ ,c(belt.col.id)])
# colnames(cleansed.data[ ,c(dumbbell.col.id)])
# colnames(cleansed.data[ ,c(forearm.col.id)])

## Cleanup some data

# Set window num as factor
#cleansed.data$num_window <- as.factor(cleansed.data$num_window)

# Row 537 contains many outliers which look like incorrect measurement.
# Imputing new values seems difficult so my approach is to remove the row.
#cleansed.data[cleansed.data$X == 5373,]$gyros_dumbbell_x <- cleansed.data[cleansed.data$X == 5373,]$gyros_dumbbell_x / 100.0
#cleansed.data[cleansed.data$X == 5373,]$gyros_dumbbell_z <- cleansed.data[cleansed.data$X == 5373,]$gyros_dumbbell_z / 100.0
cleansed.data <- cleansed.data[-5373,]

# Row X == 9274 contains an outlier in magnet_dumbbell_y which seems to be an incorrect measurement
# Fixed by imputing with average of window for magnet_dumbbell_y
cleansed.data[cleansed.data$X == 9274, ]$magnet_dumbbell_y <- 426


### More cleansing of data required eg skewness fix - but only after removal of highly correlated variables...

summary(cleansed.data)
str(cleansed.data)


## Visualisations
g <- ggplot(data = cleansed.data, aes(x = total_accel_belt, fill = user_name))
g <- g + geom_histogram(binwidth = 1)
g <- g + facet_wrap(~classe)
g
## looks like 2 modes for total_accel_belt... >12.5 or <=12.5
## --> possible feature to engineer
## --> looks like it's user dependent

g <- ggplot(data = cleansed.data, aes(x = roll_belt, fill = user_name))
g <- g + geom_histogram()
g <- g + facet_wrap(~classe)
g
## looks like 2 modes for roll_belt... >75 or <=75
## --> possible feature to engineer
## --> looks like it's user dependent

g <- ggplot(data = cleansed.data, aes(x = gyros_forearm_x, fill = user_name))
g <- g + geom_histogram()
g <- g + facet_wrap(~classe)
g
## 1 mode

g <- ggplot(data = cleansed.data, aes(x = magnet_forearm_y, fill = user_name))
g <- g + geom_histogram()
g <- g + facet_wrap(~classe)
g
## complete overlap.
## There seem to be some outliers somewhere... Fixed

g <- ggplot(data = cleansed.data, aes(x = yaw_dumbbell, fill = user_name))
g <- g + geom_histogram(binwidth = 10)
g <- g + facet_wrap(~classe)
g
## what a mess

cleansed.data$good <- as.character(cleansed.data$classe)
cleansed.data[cleansed.data$good != "A","good"] <- "Z"
cleansed.data$good <- as.factor(cleansed.data$good)

g <- ggplot(data = cleansed.data, aes(x = gyros_dumbbell_y,
                                      y = gyros_dumbbell_z,
                                      colour = good))
g <- g + geom_point()
#g <- g + facet_wrap(~classe)
g
## outlier somewhere... fixed

g <- ggplot(data = cleansed.data, aes(x = magnet_dumbbell_y,
                                      y = magnet_dumbbell_z,
                                      colour = good))
g <- g + geom_point(alpha = .5)
#g <- g + facet_wrap(~classe)
g
## outlier somewhere... fixed
## 3 clusters...

g <- ggplot(data = cleansed.data, aes(x = magnet_belt_x, fill = user_name))
g <- g + geom_histogram()
g <- g + facet_wrap(~classe)
g
## Adelmo is in a different mode...



## Create training data set
training.data <- cleansed.data[ ,c(2, arm.col.id, belt.col.id, dumbbell.col.id, forearm.col.id, 60)]

##dummies <- dummyVars(classe ~ ., data = training.data)
##head(predict(dummies, newdata = training.data))

training.data.num.only <- training.data[,-c(1, 54)]

data.cor <- cor(training.data.num.only)
corrplot(data.cor, order = "hclust")

## remove near zero variation predictors
nearZeroVar(training.data)
# none

## Remove highliy correlated predictors
high.cor.predictors <- findCorrelation(data.cor, cutoff = .75)
high.cor.predictors.name <- colnames(training.data[ , (high.cor.predictors + 1)])
training.data <- training.data[ , -(high.cor.predictors + 1)]

#corrplot(cor(training.data[,-c(1, 2, 35)]), order = "hclust")

## PCA...
pc <- princomp(training.data.num.only, cor = TRUE, scores = TRUE)
plot(pc)
biplot(pc)
screeplot(pc, type = "l")
summary(pc)

###### Explore further: factoMineR and running a model

## Model inc. users
training.idx <-createDataPartition(y=training.data$classe,p=0.5,list=FALSE)
training.set <- training.data[training.idx,]
testing.set <- training.data[-training.idx,]

rf_model<-train(x = training.set[, -36],
                y = training.set[, 36],
                data = training.set,
                method="rf",
                trControl = trainControl(method="cv",number=5),
                prox=TRUE)
print(rf_model)

test.pred <- predict(rf_model, testing.set[,-36])
confusionMatrix(test.pred,testing.set[,36])

total.pred <- predict(rf_model, training.data[,-36])
confusionMatrix(total.pred,training.data[,36])

## Model exc. users
training.set.ex.users <-  training.data[training.idx,-1]
testing.set.ex.users <-  training.data[training.idx,-1]

rf_model.ex.users <- train(x = testing.set.ex.users[, -35],
                           y = testing.set.ex.users[, 35],
                           data = testing.set.ex.users,
                           method="rf",
                           trControl = trainControl(method="cv",number=5),
                           prox=TRUE)
print(rf_model.ex.users)

test.pred.ex.users <- predict(rf_model.ex.users, testing.set.ex.users[,-35])
confusionMatrix(test.pred.ex.users,testing.set.ex.users[,35])

total.pred.ex.users <- predict(rf_model.ex.users, training.data[,-c(1,36)])
confusionMatrix(total.pred.ex.users,training.data[,36])

##### Find nice looking confusion matrix
#### Explore usage of logistic regression model


## TESTING
raw.validation.data <- read.csv("pml-testing.csv")
validation.data <- raw.validation.data

## Remove unnecessary variables
validation.data <- validation.data[, -max.col.id]
validation.data <- validation.data[, -min.col.id]
validation.data <- validation.data[, -stddev.col.id]
validation.data <- validation.data[, -var.col.id]
validation.data <- validation.data[, -kurtosis.col.id]
validation.data <- validation.data[, -skewness.col.id]
validation.data <- validation.data[, -amplitude.col.id]
validation.data <- validation.data[, -avg.col.id]

validation.data <- validation.data[ ,c(arm.col.id, belt.col.id, dumbbell.col.id, forearm.col.id)]
validation.data <- validation.data[ , -(high.cor.predictors)]

predict(rf_model.ex.users, validation.data)
