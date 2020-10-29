library(caret)
library(tidyverse)


set.seed(101)

#k folds cross validation
df_country$is_canceled = as.factor(df_country$is_canceled)
sample <- createDataPartition(df_country$is_canceled,p=0.7,list = FALSE)
train<-df_country[sample,]
testdata<-df_country[-sample,]
splitRule<- trainControl(method="repeatedcv",number=10,repeats=3,classProbs=T)

#without interaction term
#Due to high multicollinearity present between DEU and PRT, we remove DEU
glmModel <-train(is_canceled~ country_PRT+`market_segment_Online TA`+`market_segment_Offline TA/TO`
                 +market_segment_Groups+market_segment_Direct
                 +market_segment_Corporate,data=train,trControl=splitRule,method="glm")
glmTest <- predict(glmModel,newdata=testdata)
confusionMatrix(data=glmTest,testdata$is_canceled,positive="Yes")
summary(glmModel)

#method two
sample <- createDataPartition(df_country$is_canceled,p=0.7,list = FALSE)
train<-df_country[sample,]
testdata<-df_country[-sample,]
mylogit <- glm(is_canceled~ PRT_DEU+country_Others+ country_GBR
               +country_FRA+country_ESP +`market_segment_Online TA`+`market_segment_Offline TA/TO`
               +market_segment_Groups+market_segment_Direct
               +market_segment_Corporate, data = train, family = 'binomial')
summary(mylogit)

fitted.results<-predict(mylogit,newdata=testdata,type='response')
fitted.results<- ifelse(fitted.results>0.5,1,0)
misClasificError <- mean(fitted.results != testdata$is_canceled)
print(paste('Accuracy',1-misClasificError))
table(fitted.results,testdata$is_canceled)

#Strong correlation with PRT_DEU with market_segment_Groups(0.267), we add in the interaction term for this
mylogit2 <- glm(is_canceled~ PRT_DEU+country_Others+ country_GBR
               +country_FRA+country_ESP+ +`market_segment_Online TA`+`market_segment_Offline TA/TO`
               +market_segment_Groups+market_segment_Direct+ PRT_DEU*market_segment_Direct 
               + country_Others*`market_segment_Groups`, data = train, family = 'binomial')
summary(mylogit2)
anova(mylogit2, test="Chisq")

fitted.results<-predict(mylogit2,newdata=testdata,type='response')
fitted.results<- ifelse(fitted.results>0.5,1,0)
misClasificError <- mean(fitted.results != testdata$is_canceled)
print(paste('Accuracy',1-misClasificError))
table(fitted.results,testdata$is_canceled)

#including only PRT
glmModel3 <-train(is_canceled~ country_PRT+`market_segment_Online TA`+`market_segment_Offline TA/TO`
                 +market_segment_Groups+market_segment_Direct
                 +market_segment_Corporate,data=train,trControl=splitRule,method="glm")
glmTest <- predict(glmModel3,newdata=testdata)
confusionMatrix(data=glmTest,testdata$is_canceled,positive = "Yes")
summary(glmModel)

train %>%
  mutate(prob = ifelse(is_canceled == "Yes", 1, 0)) %>%
  ggplot(aes(is_canceled, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )
