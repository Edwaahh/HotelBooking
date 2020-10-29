library(caret)

set.seed(101)

na.omit(df_country)
sample <- createDataPartition(df_country$is_canceled,p=0.7,list = FALSE)
train<-df_country[sample,]
testdata<-df_country[-sample,]

fitControl<- trainControl(method="cv",number=5, savePredictions = 'final', allowParallel = TRUE)

#Randomforest model for longterm shortterm and consumer background
randomforest.model.test4<- train(is_canceled~ LOS + lead_time + is_repeated_guest + country_PRT+`market_segment_Online TA`
                                +`market_segment_Offline TA/TO`+market_segment_Groups, 
                                data=train,method="rf",tuneLength=9,trControl=fitControl)

rfModel <- varImp(randomforest.model.test)

ggplot(rfModel) + labs(title = "Random Forest Model (variable importance)")
rfModel$importance #most important variables

pred.randomForest4 <- predict(randomforest.model.test4, newdata = testdata) #predict test data

confusionMatrix(table(pred.randomForest4,testdata$is_canceled), positive = "Yes")
pred.randomForest<- ifelse(pred.randomForest>0.5,1,0)

# ada (Boosting)
fitControl <- trainControl(method = "cv",
                           number = 2,
                           savePredictions = 'final')
stacked.adaboost <- train(cancelled ~ lead_time + country_PRT + total_stay + market_segment,
                          data = df_country,
                          method = 'adaboost',
                          trControl=fitControl)

pred.stackedada <- predict(stacked.adaboost, newdata=testdata) #predict test data
confusionMatrix(table(pred.stackedada, testdata$cancelled),positive = "Yes")
summary(stacked.adaboost)
ggplot(stacked.adaboost)
varimp.stacked.adaboost <- varImp(stacked.adaboost)
ggplot(varimp.stacked.adaboost) + labs(title ="Hypo 4: Adaboost Model (variable importance)")