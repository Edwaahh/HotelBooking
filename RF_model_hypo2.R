library(caret)

set.seed(101)

na.omit(df_country)
sample <- createDataPartition(df_country$is_canceled,p=0.7,list = FALSE)
train<-df_country[sample,]
testdata<-df_country[-sample,]

fitControl<- trainControl(method="cv",number=5, savePredictions = 'final', allowParallel = TRUE)

randomforest.model.test<- train(is_canceled~ country_PRT+country_Others+ country_GBR
                                +country_FRA+country_ESP+`market_segment_Online TA`+`market_segment_Offline TA/TO`
                                +market_segment_Groups+market_segment_Direct
                                +market_segment_Corporate, data=train,method="rf",tuneLength=9,trControl=fitControl)

rfModel <- varImp(randomforest.model.test)

ggplot(rfModel) + labs(title = "Random Forest Model (variable importance)")
rfModel$importance #most important variables

pred.randomForest <- predict(randomforest.model.test, newdata = testdata) #predict test data

confusionMatrix(table(pred.randomForest,testdata$is_canceled))
pred.randomForest<- ifelse(pred.randomForest>0.5,1,0)
misClasificError <- mean(pred.randomForest != testdata$is_canceled)
print(paste('Accuracy',1-misClasificError))

#model 2 without unimportant variables
randomforest.model.test<- train(is_canceled~ country_PRT+`market_segment_Online TA`+`market_segment_Offline TA/TO`
                                +market_segment_Groups, data=train,method="rf",tuneLength=4,trControl=fitControl)

rfModel <- varImp(randomforest.model.test)

ggplot(rfModel) + labs(title = "Random Forest Model (variable importance)")
rfModel$importance #most important variables

pred.randomForest <- predict(randomforest.model.test, newdata = testdata) #predict test data

confusionMatrix(table(pred.randomForest,testdata$is_canceled), positive = "Yes")
pred.randomForest<- ifelse(pred.randomForest>0.5,1,0)

