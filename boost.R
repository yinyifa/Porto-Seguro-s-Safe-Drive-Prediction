
# import train
train <- read.table("../project/train.csv", sep = ",", header = T)

# import test
test <- read.table("../project/test.csv", sep = ",", header = T)


# combine sets for transformation
# create combine-columns
test$target <- NA
train$type <- 1
test$type <- 0

data <- rbind(train, test)
data$type <- as.integer(as.character(data$type))

# recode NAs (NAs == -1)
str(data)
which(sapply(data, min) == -1)
require(car)

for (i in which(sapply(data, min) == -1)){
  data[, i] <- recode(data[, i], '"-1" = NA')
}

# transform "id", bin" and "cat" to factors
data$id <- factor(data$id)
for (i in grep("cat|bin", colnames(data))){
  data[, i] <- factor(data[, i])
}
str(data)

################################
# Exploratory analysis
#require(corrplot)
#which(sapply(data, is.factor))
#corrplot(cor(train[,-c(1,60,which(sapply(data, is.factor)))]))

# highly correlated are "ps_reg_01", "ps_reg_02" and "ps_reg_03"
# and "ps_car_12", "ps_car_13" and "ps_car_15"

################################
# Feature engeneering
# clustering reg + car
which(sapply(data, anyNA))
c("ps_reg_01", "ps_reg_02", "ps_reg_03",
  "ps_car_12", "ps_car_13", "ps_car_15") %in% colnames(data[,which(sapply(data, anyNA))])

# kmeans only without NAs
set.seed(17)
kmeans.3 <- kmeans(data[,c("ps_reg_01", "ps_reg_02", "ps_car_13", "ps_car_15")], 3)

data$kmeans.3 <- kmeans.3$cluster

prop.table(table(kmeans.3$cluster[1:nrow(train)], train$target), margin = 2)
prop.table(table(kmeans.3$cluster[1:nrow(train)], train$target))

set.seed(19)
kmeans.calc <- kmeans(data[,which(grepl("ps_calc", colnames(data)))], 3)
data$kmeans.calc <- kmeans.calc$cluster

prop.table(table(kmeans.calc$cluster[1:nrow(train)], train$target), margin = 2)
prop.table(table(kmeans.calc$cluster[1:nrow(train)], train$target))

data <- data[,-which(grepl("ps_calc", colnames(data)))]

# sum up reg and car features
data$cor_reg <- rowSums(cbind(data[,"ps_reg_01"], data[,"ps_reg_02"], data[,"ps_reg_03"]), na.rm = T)
#data$cor_car <- rowSums(cbind(data[,"ps_car_12"], data[,"ps_car_13"], data[,"ps_car_15"]), na.rm = T)
data$car_reg <- (data[,"cor_reg"]^2)*sqrt(data[,"ps_car_13"])*data[,"ps_reg_03"]

rm(kmeans.3, kmeans.calc, i)

################################

# split data into train and test
train <- data[which(data$type == 1),]
test <- data[which(data$type == 0),]

# remove "combine-columns"
train$type = NULL
test$type = NULL
test$target = NULL

########################################################################
########################################################################
install.packages("xgboost")
require(xgboost)
install.packages("caret")
require(caret)
install.packages("MLmetrics")
require(MLmetrics) # for NormalizedGini function

train <- data.frame(lapply(train, function(x) as.numeric(as.character(x))))
test <- data.frame(lapply(test, function(x) as.numeric(as.character(x))))

#########################################################

label_train <- train[,"target"]

xgb_train <- xgb.DMatrix(data = as.matrix(train[,-c(1,2)]), label = label_train)
xgb_test <- xgb.DMatrix(data = as.matrix(test[,-1]))

#########################################################
#########################################################

# wrap up into a function to be called within xgboost.train
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

#########################################################
# CV
#start.time <- Sys.time()
#set.seed(17)
#xgb.model <- xgb.cv(data = xgb_train,
#                    nfold = 5,
#                    nrounds = 1000, 
#                    stratified = TRUE,
#                    params = xgb_params,
#                    early_stopping_rounds = 20,
#                    print_every_n = 5,
#                    feval = normalizedgini,
#                    maximize = TRUE)

# measure time
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken  


#########################################################
## train all data:
# best nrounds
#best_nrounds = as.integer(xgb.model$best_iteration / (1 - 1 / xgb.model$call$nfold))
best_nrounds = 679
#best_nrounds = as.integer(848 / (1 - 1 / 5))
# measure time
start.time <- Sys.time()

set.seed(17)
xgb.submit_model <- xgb.train(data = xgb_train, 
                              nrounds = best_nrounds, 
                              #                              nrounds = xgb.model$best_iteration, 
                              watchlist = list(train = xgb_train),
                              feval = normalizedgini,
                              params = xgb_params,
                              print_every_n = 5)

# measure time
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken  

xgb.submit_model

# feature importance
importance_matrix <- xgb.importance(dimnames(train[,-c(1,2)])[[2]], model=xgb.submit_model)
xgb.plot.importance(importance_matrix)

# predict test
xgb.pred_prob <- predict(xgb.submit_model, xgb_test)
print(summary(xgb.pred_prob))


# export
export <- data.frame(id = as.integer(test[, "id"]), target = as.numeric(xgb.pred_prob))
any(is.na(export))

# export
write.table(export, "submit_xgb.csv", row.names = F, sep = ",", dec = ".")