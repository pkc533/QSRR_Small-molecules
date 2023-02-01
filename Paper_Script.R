#Load required packages

library("caret")
library("caretEnsemble")
library("ggplot2")
library("gridarrange")

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



### Full dataset 

allF_lasso <- train(tR~., data=train, method="glmnet",metric=metric,preProc=c("center","scale"),trControl=trainControl)
allF_rf <- train(tR~., data=train, method="rf", metric=metric,importance = T, preProc=c("center","scale"),
                 trControl=trainControl)
allF_gbm <- train(tR~., data=train, method="gbm", metric=metric,preProc=c("center","scale"),
                  trControl=trainControl)
#COmbining predictions
results <- resamples(list(mlr_cfs = MLR_CFS, svr_cfs = SVR_CFS, RFE_mlr = MLR_RFE, 
                              RFE_svr = SVR_RFE,lasso = allF_lasso, rf = allF_rf, gbm = allF_gbm))
summary(results)

#ENsemble model predictions
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

############# Stacking Models #####################################################

fit.stacknew <- train(tR~., data=df, method="lm", metric=metric,trControl=trainControl)

results_mods <- resamples(list(mlr_cfs = MLR_CFS, svr_cfs = SVR_CFS, RFE_mlr = MLR_RFE, 
                                  RFE_svr = SVR_RFE,lasso = allF_lasso, 
                                  rf = allF_rf, gbm = allF_gbm, stack = fit.stacknew))
summary(results_mods)
pred_stack <- predict(fit.stacknew, test[-1])

## Saving model performances
#MLR_CFS
rmse_MLRCFS <- RMSE(pred_MLR_CFS, CFS_te220$tR)
r2_MLRCFS <- R2(pred_MLR_CFS, CFS_te220$tR)
#SVR_CFS
rmse_SVRCFS <- RMSE(pred_SVR_CFS, CFS_te220$tR)
r2_SVRCFS <- R2(pred_SVR_CFS, CFS_te220$tR)
#MLR_RFE
rmse_MLRRFE <- RMSE(pred_MLR_RFE, CFS_te220$tR)
r2_MLRRFE <- R2(pred_MLR_RFEpred_MLR_CFS, CFS_te220$tR)
#SVR_RFE               
rmse_SVRRFE <- RMSE(pred_SVR_RFE, CFS_te220$tR)
r2_SVRRFE <- R2(pred_SVR_RFE, CFS_te220$tR)
#Lasso
rmse_lasso <- RMSE(pred_lasso, CFS_te220$tR)
r2_lasso<- R2(pred_lasso, CFS_te220$tR)
#Random Forest
rmse_rf <- RMSE(pred_rf, CFS_te220$tR)
r2_rf <- R2(pred_rf, CFS_te220$tR)
#GBM
rmse_gbm <- RMSE(pred_gbm, CFS_te220$tR)
r2_gbm <- R2(pred_gbm, CFS_te220$tR)
#Stacking
rmse_stack <- RMSE(pred_stcak, CFS_te220$tR)
r2_stack <- R2(pred_stack, CFS_te220$tR)

#Combining all performances
Model_performances = cbind(rmse_MLRCFS,rmse_SVRCFS,
                     rmse_MLRRFE,rmse_SVRRFE,
                     rmse_lasso, rmse_rf,rmse_gbm,rmse_stack,
                     r2_MLRCFS,r2_SVRCFS, 
                     r2_MLRRFE,r2_SVRRFE,
                     r2_lasso,r2_rf,r2_gbm,r2_stack,
                     r2_MLRCFS,r2_SVRCFS,
                     r2_MLRRFE, r2_SVRRFE,
                     r2_lasso,r2_rf,r2_gbm,r2_stack)



write.csv(Model_performances,"model_performances.csv")

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







