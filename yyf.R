install.packages("e1071")
install.packages("randomForest")
library(e1071)
library(class)
library(randomForest)

mytraindata<-read.csv(file="C:/Users/yinyi/Box Sync/B365 (Siyi Xian)/train.csv",header=TRUE,sep=",")
mytraindata<-as.matrix(mytraindata)
mytestdata<-read.csv(file="C:/Users/yinyi/Box Sync/B365 (Siyi Xian)/test.csv",header=TRUE,sep=",")
mytestdata<-as.matrix(mytestdata)
myresult<-read.csv(file="C:/Users/yinyi/Box Sync/B365 (Siyi Xian)/sample_submission.csv",header=TRUE,sep=",")
myresult<-as.matrix(myresult)

##data preprocessing

mytraindata=mytraindata[,2:59]
mytraindata=mytraindata[,1:38]
mytraindata0=matrix(nrow = 573518,ncol = 38)
mytraindata1=matrix(nrow=21694,ncol=38)
a=1
b=1

for (i in 1:nrow(mytraindata)) {
  if(mytraindata[i,1]==0){
    mytraindata0[a,]=mytraindata[i,]
    a=a+1
  }else{
    mytraindata1[b,]=mytraindata[i,]
    b=b+1
  }
}



## data normalization, decrease prediction's error rate
# for (i in 3:(ncol(mytraindata))) {
#   mytraindata[,i]<-as.numeric(mytraindata[,i])/(as.numeric(max(mytraindata[,i]))/10)
# }

## random disorder data set, decrease prediction's error rate 
mytraindata<-mytraindata[sample(nrow(mytraindata)),]

## k-folds implementation## learn from stackexchange:
## How to split a data set to do 10-fold cross validation
testformydata<-list();
trainformydata<-list();

folds <- cut(seq(1,nrow(mytraindata)),breaks=5,labels=FALSE)
for(i in 1:5){
  testNumber <- which(folds==i)
  testformydata[[i]] <- mytraindata[testNumber, ]
  trainformydata[[i]] <- mytraindata[-testNumber, ]
}

error <- c()
result <- list()
test <- list()

for (i in 1:5) {
  m <-naiveBayes(trainformydata[[i]][, -1], as.factor(trainformydata[[i]][, 1]))
  result[[i]] <- predict(m, testformydata[[i]],"raw")[,2]
}


for (i in 1:5) {
  test[[i]] <- testformydata[[i]][, 1]
}

for (i in 1:5) {
  errortemp = 0
  for (j in 1:length(result[[i]])) {
      errortemp = errortemp + abs(result[[i]][j]-test[[i]][j])}
  error[i] <- errortemp / length(result[[i]])
}

plot(
  error,
  xlab = "# folds",
  ylab = "average error rate",
  main = "nb",
  frame.plot = F,
  type = "l",
  col = "green"
)


n<-naiveBayes(trainformydata[[3]][, -1], as.factor(trainformydata[[3]][, 1]))
myresult[,2] <- predict(n, mytestdata[,2:39],"raw")[,2]
# for (i in 1:nrow(myresult)) {
#         if(myresult[i,2]==1){
#           myresult[i,2]=0
#         }else{
#           myresult[i,2]=1
#         }
# }

# mytestdata00=mytestdata[,2:39]
# myresult1=myresult
# for (i in 1:nrow(mytestdata00)) {
#   
#   for(j in 1:38){
#   }
#   if(){
#   myresult1[i,2]=
#   }else{
#   myresult1[i,2]=
# }}

write.csv(myresult, file = "myresult.csv",row.names = F)
 # newdata<-mytraindata[,2:59]
 # m <-
 #   naiveBayes(newdata[,2:58], as.factor(newdata[, 1]))
 # myresult[,2] <- predict(m, mytestdata[,2:58])
 # for (i in 1:nrow(myresult)) {
 #   if(myresult[i,2]==1){
 #     myresult[i,2]=0
 #   }else{
 #     myresult[i,2]=1
 #   }
 # }
 
# error <- sum(abs(myresult[, 2] - mytestdata[, 2]))

# for (i in 1:nrow(myresult)) {
#      if(myresult[i,2]==1){
#        myresult[i,2]=0
#      
#    }




# pca = prcomp(newdata, center = TRUE)
# std_dev <- pca$sdev
# pr_var <- std_dev ^ 2
# prop_varex <- pr_var / sum(pr_var)
# 
# ##1.1
# plot(
#   pca$x[, 1],
#   pca$x[, 2],
#   main = "PC1 and PC2",
#   xlab = "PC1",
#   ylab = "PC2",
#   col = "red"
# )
# 
# ##1.2
# # curve bends at 7th
# plot(prop_varex,
#      xlab = "Principal Component",
#      ylab = "Proportion of Variance Explained",
#      type = "b")
# # 9th over 75%
# plot(cumsum(prop_varex),
#      xlab = "Principal Component",
#      ylab = "Cumulative Proportion of Variance Explained",
#      type = "b")
# cumsum(prop_varex)
# # 1 and 2's variance over 1
# plot(pr_var,
#      xlab = "Principal Component",
#      ylab = "Variance",
#      type = "b")
# 
# ##1.4
# # Perform dimensionality reduction over Ionosphere data set with PCA.
# # Keep 99% of variance
# # 6th over 95
# delta_r <- pca$x[, 1:6] %*% t(pca$rotation[, 1:6])
# delta_r <- scale(delta_r, center = -1 * pca$center, scale = FALSE)
# k <- kmeans(delta_r, centers = 2)
# d <- matrix(nrow = nrow(delta_r), ncol = 1)
# for (j in 1:nrow(delta_r)) {
#   d[j, ] <- sum(delta_r[j, ])
# }
# 
# #for (i in 1:nrow(myresult)) {
# #  myresult[i,2]<-d[i,]/300
# #}
 # myresult1 <- matrix(nrow = nrow(mytraindata), ncol = 1)
 # for (i in 1:nrow(mytraindata)) {
 #   myresult1[i, 1] <-
 #     (
 #       mytraindata[i, 21] + mytraindata[i, 22] / 2 + abs(mytraindata[i, 23]) /
 #         5 + abs(mytraindata[i, 36]) / 2 + mytraindata[i, 37] / 4 + abs(mytraindata[i, 38]) +
 #         mytraindata[i, 39] / 4
 #     ) / 7
 # }
 #  for (j in 1:nrow(myresult1)) {
 #    if (myresult1[j, 1] > 0.5) {
 #      myresult1[j, 1] <- 1
 #    }
 #    else{
 #      myresult1[j, 1] <- 0
 #    }
 #  }
 # 
 # error <- sum(abs(myresult1[, 1] - mytraindata[, 2]))
 # 
 # for (i in 1:nrow(myresult)) {
 #   myresult[i,2]<-(mytestdata[i,20]/0.9+mytestdata[i,21]/1.9+abs(mytestdata[i,22])/3+mytestdata[i,35]/1.3+mytestdata[i,36]/3+abs(mytestdata[i,37])+mytestdata[i,38]/3.75)/7
 # }

 # for (j in 1:nrow(myresult)) {
 #   if(myresult[j,2]>0.55){
 #     myresult[j,2]<-1
 #   }
 #   else{
 #     myresult[j,2]<-0
 #   }
 # }

