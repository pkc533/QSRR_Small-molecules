#Load required packages

library("caret")
library("caretEnsemble")
library("ggplot2")
library("gridarrange")

#Wrapper method(RFE) feature selection
#Read the data
train = read.csv("train_data.csv")
test= read.csv("test_data.csv")
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# summarize the results
features <- rfe(x, train[,1],sizes=c(2:145), rfeControl=control)
#list the chosen features
predictors(features)
features_rf = (predictors(features))
RFE_features = train[,(colnames(train) %in% features_rf)]

#MODEL PREDICTIONS    
trainControl <- trainControl(method="cv", number=10, savePredictions = TRUE)
metric <- "RMSE"

##CFS models(Data with features selected based on correlations)

CFS_tr220 = read.csv("CFS_tr220.csv")
CFS_te220 = read.csv("CFS_te220.csv")

#CFS-Models
MLR_CFS <- train(tR~., data=CFS_tr220, method="lm", metric=metric,
                 preProc=c("center","scale"),trControl=trainControl)
SVR_CFS <- train(tR~., data=CFS_tr220, method="svmRadial", metric=metric,trControl = trainControl,
                 preProc=c("center","scale"), tunegrid = grid)
pred_MLR_CFS <- predict(MLR_CFS, CFS_te220)
pred_SVR_CFS <- predict(SVR_CFS, CFS_te220)

#[2]RFE-Models
RFE_tr220 = read.csv("RFE_tr220.csv")
RFE_te220 = read.csv("RFE_te220.csv")
MLR_RFE <- train(tR~., data=RFE_tr220, method="lm", metric=metric,
                 preProc=c("center","scale"),trControl=trainControl)
SVR_RFE <- train(tR~., data=RFE_tr220, method="svmRadial", metric=metric,trControl = trainControl,
                 preProc=c("center","scale"), tunegrid = grid)
pred_MLR_RFE <- predict(MLR_RFE, RFE_te220)
pred_SVR_RFE <- predict(SVR_RFE, RFE_te220)


#Models with embedded feature selections
### Full dataset 

allF_lasso <- train(tR~., data=train, method="glmnet",metric=metric,preProc=c("center","scale"),trControl=trainControl)
allF_rf <- train(tR~., data=train, method="rf", metric=metric,importance = T, preProc=c("center","scale"),
                 trControl=trainControl)
allF_gbm <- train(tR~., data=train, method="gbm", metric=metric,preProc=c("center","scale"),
                  trControl=trainControl)
#Combining predictions
results <- resamples(list(mlr_cfs = MLR_CFS, svr_cfs = SVR_CFS, RFE_mlr = MLR_RFE, 
                              RFE_svr = SVR_RFE,lasso = allF_lasso, rf = allF_rf, gbm = allF_gbm))
summary(results)

#Ensemble model predictions
pred_lasso <- predict(allF_lasso, test_zero220)
pred_rf <- predict(allF_rf, test_zero220)
pred_gbm <- predict(allF_gbm, test_zero220)

#Saving model predictions

predictions_ext_Val = cbind(test$tR, pred_MLR_CFS, pred_SVR_CFS,pred_MLR_RFE,
                 pred_SVR_RFE, pred_lasso, pred_rf, pred_gbm)
##Saving CV Predictions
CVPred_MLR_CFS = MLR_CFS$pred
CVPred_SVR_CFS = SVR_CFS$pred
CVPred_MLR_RFE = MLR_RFE$pred
CVPred_SVR = SVR_RFE$pred
CVPred_allF_lasso = allF_lasso$pred
CVPred_allF_gbm = allF_gbm$pred

CVPred_train = cbind(train$tR, CVPred_MLR_CFS,CVPred_SVR_CFS,CVPred_MLR_RFE,CVPred_SVR,CVPred_allF_lasso,CVPred_allF_gbm)
#Stacking
df = read.csv("CV_pred_train.csv")
test = read.csv("predictions_ext_Val.csv")

##### Stacking Models ######

fit.stacknew <- train(tR~., data=df, method="lm", metric=metric,trControl=trainControl)

results_mods <- resamples(list(mlr_cfs = MLR_CFS, svr_cfs = SVR_CFS, RFE_mlr = MLR_RFE, 
                                  RFE_svr = SVR_RFE,lasso = allF_lasso, 
                                  rf = allF_rf, gbm = allF_gbm, stack = fit.stacknew))
summary(results_mods)
pred_stack <- predict(fit.stacknew, test[-1])


#Prediced vs. Experimental retention times plots
obs=df$tR
pred=df$MLR_CFS #(This will vary from one model predicion to other)

p1 <- ggplot(data = df1,aes(x=obs, y=pred),size= 2.2)+
  geom_point(aes(x=obs, y=pred,colour = Label,shape = Label),size=2.2) +
  geom_abline(intercept = 0, color="black", 
              linetype="dotted", size=0.9)+ 
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("MLR_CFS")+
  xlab("Observed tR(min)") + ylab("Predicted tR(min)")+
  theme(text = element_text(size=20))+theme(plot.title = element_text(hjust = 1,size = 20))







