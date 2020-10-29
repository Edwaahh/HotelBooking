#For consumer background and long term vs short term

#k folds cross validation
df_country$is_canceled = as.factor(df_country$is_canceled)
sample <- createDataPartition(df_country$is_canceled,p=0.7,list = FALSE)
train<-df_country[sample,]
testdata<-df_country[-sample,]
splitRule<- trainControl(method="repeatedcv",number=10,repeats=3,classProbs=T)

#without interaction term
#Due to high multicollinearity present between DEU and PRT, we remove DEU
glmModel <-train(is_canceled~ LOS+ lead_time + is_repeated_guest +country_PRT+country_ESP +`market_segment_Online TA`+`market_segment_Offline TA/TO`
                 +market_segment_Groups+market_segment_Direct
                 +market_segment_Corporate,data=train,trControl=splitRule,method="glm")

glmTest <- predict(glmModel,newdata=testdata)
confusionMatrix(data=glmTest,testdata$is_canceled)
summary(glmModel)

glmModel2 <- train(is_canceled~ LOS+ lead_time+ is_repeated_guest +country_PRT 
                   +`market_segment_Online TA`+`market_segment_Offline TA/TO`
                   +market_segment_Groups,data=train,trControl=splitRule,method="glm")
glmTest2 <- predict(glmModel2,newdata=testdata)
confusionMatrix(data=glmTest2,testdata$is_canceled,positive="Yes")
summary(glmModel2)

#without interaction term
#Due to high multicollinearity present between DEU and PRT, we remove DEU
glmMode.interaction <-train(is_canceled~ lead_time +country_PRT +`market_segment_Online TA`
                 +market_segment_Groups,data=train,trControl=splitRule,method="glm")

glmTest.interaction <- predict(glmMode.interaction,newdata=testdata)
confusionMatrix(data=glmTest.interaction,testdata$is_canceled, positive = "Yes")
summary(glmMode.interaction)

summary(glmModel2)