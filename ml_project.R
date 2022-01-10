library(caret);library(randomForest);library(PRP)
library(rpart);library(rpart.plot);library(corrplot)

# Load data sets and set all "N/A","NA","#DIV/0!","" to "NA"
train = read.csv("~/R/Data_science_course/8 ML/8_ML_course_project/pml-training.csv",na.strings=c("N/A","NA","#DIV/0!",""))
test = read.csv("~/R/Data_science_course/8 ML/8_ML_course_project/pml-testing.csv",na.strings=c("N/A","NA","#DIV/0!",""))

# View data
View(test)

#column 1-7 are not needed
train <- train[,8:length(colnames(train))]
test <- test[,8:length(colnames(test))]

#also remove columns with NA
train_clean <- train[, colSums(is.na(train)) == 0] 
test_clean <- test[, colSums(is.na(test)) == 0] 

#split train-set into training and validation
inTrain <- createDataPartition(train_clean$classe,p=0.7, list=FALSE)
training <- train_clean[inTrain,]
validation <- train_clean[-inTrain,]

#use random forest to find model
control.parms <- trainControl(method="cv", 5)
rf_model <- train(classe ~ ., data=training, method="rf",trControl=control.parms, ntree=251)
rf_model

#test model on validation set & remember to factor validation set...
predict <- predict(rf_model, validation)
confusionMatrix(as.factor(validation$classe),predict)

# predict on test set
ptest <- predict(rf_model, test_clean)
ptest

# graphical tree plot
rf_model_plot <- rpart(classe ~ ., data=train_clean, method="class")
prp(rf_model_plot)