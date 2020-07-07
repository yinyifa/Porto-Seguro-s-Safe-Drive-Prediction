install.packages("xgboost")
install.packages("MLmetrics")
install.packages("corrplot")
require(xgboost)
require(MLmetrics)
require(corrplot)

#learn from https://www.kaggle.com/nonserial/porto-safedriver-ii-final/code
# import data
train <- read.table("C:/Users/yinyi/Box Sync/B365 (Siyi Xian)/train.csv", sep = ",", header = T)
test <- read.table("C:/Users/yinyi/Box Sync/B365 (Siyi Xian)/test.csv", sep = ",", header = T)

#data preprocessing
###################################################
corrplot(cor(train))

###delete all the col with "ps_calc"

train <- train[,-which(grepl("ps_calc", colnames(train)))]
test  <- test[,-which(grepl("ps_calc", colnames(test)))]


train[train == -1] <- NA
which(sapply(train, anyNA))


test[test== -1] <- NA
#######################################################################################
#replace the train missing value with mean
train$ps_ind_02_cat[is.na(train$ps_ind_02_cat)]<-mean(train$ps_ind_02_cat, na.rm = TRUE)
train$ps_ind_04_cat[is.na(train$ps_ind_04_cat)]<-mean(train$ps_ind_04_cat, na.rm = TRUE)
train$ps_ind_05_cat[is.na(train$ps_ind_05_cat)]<-mean(train$ps_ind_05_cat, na.rm = TRUE)

train$ps_reg_03[is.na(train$ps_reg_03)]<-mean(train$ps_reg_03, na.rm = TRUE)

train$ps_car_01_cat[is.na(train$ps_car_01_cat)]<-mean(train$ps_car_01_cat,na.rm = TRUE)
train$ps_car_02_cat[is.na(train$ps_car_02_cat)]<-mean(train$ps_car_02_cat,na.rm = TRUE)
train$ps_car_03_cat[is.na(train$ps_car_03_cat)]<-mean(train$ps_car_03_cat,na.rm = TRUE)
train$ps_car_05_cat[is.na(train$ps_car_05_cat)]<-mean(train$ps_car_05_cat,na.rm = TRUE)
train$ps_car_07_cat[is.na(train$ps_car_07_cat)]<-mean(train$ps_car_07_cat,na.rm = TRUE)
train$ps_car_09_cat[is.na(train$ps_car_09_cat)]<-mean(train$ps_car_09_cat,na.rm = TRUE)
train$ps_car_11[is.na(train$ps_car_11)]<-mean(train$ps_car_11,na.rm = TRUE)
train$ps_car_12[is.na(train$ps_car_12)]<-mean(train$ps_car_12,na.rm = TRUE)
train$ps_car_14[is.na(train$ps_car_14)]<-mean(train$ps_car_14,na.rm = TRUE)



anyNA(train)
which(sapply(train, anyNA))
##################################################


label_train <- train[,"target"]

xgb_train <- xgb.DMatrix(data = as.matrix(train[,-c(1,2)]), label = label_train)
xgb_test <- xgb.DMatrix(data = as.matrix(test[,-1]))

normalizedgini <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  score <- NormalizedGini(y_true = labels, y_pred = preds)
  return(list(metric = "NormalizedGini", value = score))
}
#########################################################

#########################################################
xgb_params <- list(booster = "gbtree",
                   objective = "binary:logistic",
                   nthread = 4,
                   eta = 0.04,
                   gamma = 10,
                   min_child_weight = 6,
                   subsample = 0.8,
                   colsample_bytree = 0.9,
                   max_depth = 4,
                   scale_pos_weight = 1.6,
                   reg_alpha = 8,
                   reg_lambda = 1.3,
                   base_score = mean(label_train))


best_nrounds = 1000

xgb.submit_model <- xgb.train(data = xgb_train, 
                              nrounds = best_nrounds, 
                              #                              nrounds = xgb.model$best_iteration, 
                              watchlist = list(train = xgb_train),
                              feval = normalizedgini,
                              params = xgb_params,
                              print_every_n = 5)



xgb.pred_prob <- predict(xgb.submit_model, xgb_test)


# export
write.table(export, "myresult.csv", row.names = F, sep = ",", dec = ".")
