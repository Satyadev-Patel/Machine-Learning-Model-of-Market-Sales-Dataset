path <- "D:\academics\courses\ML nuclei'\intern proj 1"
setwd(path)
#load data
train <- read.csv("train_Big.csv")
test <- read.csv("test_Big.csv")
#create a new variable in test file
test$Item_Outlet_Sales <- 1
#combine train and test data
combi <- rbind(train, test)
#impute missing value in Item_Weight
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
#impute 0 in item_visibility
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),
                                  combi$Item_Visibility)
##rename level in Outlet_Size
levels(combi$Outlet_Size)[1] <- "Other"
#rename levels of Item_Fat_Content
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" =
                                                               "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
#create a new column 2013 - Year
combi$Year <- 2013 - combi$Outlet_Establishment_Year
library(dummies)
combi <- dummy.data.frame(combi, names = c('Outlet_Size','Outlet_Location_Type','Outlet_Type'),
                          sep='_')
#drop variables not required in modeling
library(dplyr)
combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))
#divide data set
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]
#linear regression
linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = new_train)
library(Metrics)
rmse(new_train$Item_Outlet_Sales, exp(linear_model$fitted.values))
library(rpart)
library(e1071)
library(rpart.plot)
library(caret)
#setting the tree control parameters
fitControl <- trainControl(method = "cv", number = 5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)
#decision tree
tree_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "rpart", trControl = fitControl, tuneGrid
                      = cartGrid)
print(tree_model)
main_tree <- rpart(Item_Outlet_Sales ~ ., data = new_train, control = rpart.control(cp=0.01))
pre_score <- predict(main_tree, type = "vector")
library(randomForest)
control <- trainControl(method = "cv", number = 5)
rf_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "parRF", trControl = control, prox = TRUE, allowParallel = TRUE)
print(rf_model)
ny<-data.frame(new_train)
forest_model <- randomForest(Item_Outlet_Sales ~ ., data = ny, mtry = 16, ntree = 1000)
varImpPlot(forest_model)
ny1<-data.frame(new_test)
main_predict <- predict(main_tree, newdata = ny1, type = "vector")
sub_file <- data.frame(Item_Identifier = test$Item_Identifier, Outlet_Identifier = test$Outlet_Identifier,
                       Item_Outlet_Sales = main_predict)
write.csv(sub_file, 'DecisionTree_sales.csv')