library(class)

mytraindata <-read.csv(file = "../project/train.csv")
#mytraindata <- as.matrix(mytraindata)
mytestdata <-read.csv(file = "../project/test.csv")
#mytestdata <- as.matrix(mytestdata)


#mytraindata[mytraindata == -1] <- NA
#mytestdata[mytestdata== -1] <- NA

#mytestdata$ps_ind_02_cat[is.na(mytestdata$ps_ind_02_cat)]<-mean(mytestdata$ps_ind_02_cat)
#mytestdata$ps_ind_04_cat[is.na(mytestdata$ps_ind_04_cat)]<-mean(mytestdata$ps_ind_04_cat)
#mytestdata$ps_ind_05_cat[is.na(mytestdata$ps_ind_05_cat)]<-mean(mytestdata$ps_ind_05_cat)
#mytestdata$ps_car_01_cat[is.na(mytestdata$ps_car_01_cat)]<-mean(mytestdata$ps_car_01_cat)
#mytestdata$ps_car_02_cat[is.na(mytestdata$ps_car_02_cat)]<-mean(mytestdata$ps_car_02_cat)
#mytestdata$ps_car_07_cat[is.na(mytestdata$ps_car_07_cat)]<-mean(mytestdata$ps_car_07_cat)
#mytestdata$ps_car_09_cat[is.na(mytestdata$ps_car_09_cat)]<-mean(mytestdata$ps_car_09_cat)
#mytestdata$ps_car_11[is.na(mytestdata$ps_car_11)]<-mean(mytestdata$ps_car_11)
#mytestdata$ps_car_12[is.na(mytestdata$ps_car_12)]<-mean(mytestdata$ps_car_12,na.rm=T)
#mytestdata$ps_car_14[is.na(mytestdata$ps_car_14)]<-mean(mytestdata$ps_car_14,na.rm=T)
#mytraindata$ps_ind_02_cat[is.na(mytraindata$ps_ind_02_cat)]<-mean(mytraindata$ps_ind_02_cat)
#mytraindata$ps_ind_04_cat[is.na(mytraindata$ps_ind_04_cat)]<-mean(mytraindata$ps_ind_04_cat)
#mytraindata$ps_ind_05_cat[is.na(mytraindata$ps_ind_05_cat)]<-mean(mytraindata$ps_ind_05_cat)
#mytraindata$ps_car_01_cat[is.na(mytraindata$ps_car_01_cat)]<-mean(mytraindata$ps_car_01_cat)
#mytraindata$ps_car_02_cat[is.na(mytraindata$ps_car_02_cat)]<-mean(mytraindata$ps_car_02_cat)
#mytraindata$ps_car_07_cat[is.na(mytraindata$ps_car_07_cat)]<-mean(mytraindata$ps_car_07_cat)
#mytraindata$ps_car_09_cat[is.na(mytraindata$ps_car_09_cat)]<-mean(mytraindata$ps_car_09_cat)
#mytraindata$ps_car_11[is.na(mytraindata$ps_car_11)]<-mean(mytraindata$ps_car_11)
#mytraindata$ps_car_12[is.na(mytraindata$ps_car_12)]<-mean(mytraindata$ps_car_12,na.rm=T)
#mytraindata$ps_car_14[is.na(mytraindata$ps_car_14)]<-mean(mytraindata$ps_car_14,na.rm=T)

#randomly drop
#count=0
#jls=c()
#for (j in 1:573518){
#  if(mytraindata[j,2]==0 & count == 4){
#    count = 0
#  }
#  else if(mytraindata[j,2]==0){
#    jls<-c(jls,j)
#    count = count + 1
#  }
#}
#mytraindata <- mytraindata[-jls, ]
##normalize
for (i in 3:59)
  mytraindata[, i] = mytraindata[, i]/max(mytraindata[, i])

for (i in 2:58)
  mytestdata[, i] = mytestdata[, i]/max(mytestdata[, i])

##based on important
mytraindata$ps_car_13=mytraindata$ps_car_13*17
mytraindata$ps_car_06_cat=mytraindata$ps_car_06_cat*14
mytraindata$ps_car_14=mytraindata$ps_car_14*12
mytraindata$ps_calc_10=mytraindata$ps_calc_10*10
mytraindata$ps_car_01_cat=mytraindata$ps_car_01_cat*10
mytraindata$ps_ind_15=mytraindata$ps_ind_15*10
mytraindata$ps_calc_14=mytraindata$ps_calc_14*10
mytraindata$ps_ind_03=mytraindata$ps_ind_03*9
mytraindata$ps_calc_11=mytraindata$ps_calc_11*9
mytraindata$ps_red_02=mytraindata$ps_reg_02*9
mytraindata$ps_calc_01=mytraindata$ps_calc_01*9
mytraindata$ps_calc_03=mytraindata$ps_calc_03*9
mytraindata$ps_calc_02=mytraindata$ps_calc_02*9
mytraindata$ps_calc_13=mytraindata$ps_calc_13*8
mytraindata$ps_car_15=mytraindata$ps_car_15*8
mytraindata$ps_calc_08=mytraindata$ps_calc_08*7
mytraindata$ps_calc_07=mytraindata$ps_calc_07*7
mytraindata$ps_reg_01=mytraindata$ps_reg_01*7
mytraindata$ps_calc_06=mytraindata$ps_calc_06*7
mytraindata$ps_ind_01=mytraindata$ps_ind_01*7
mytraindata$ps_calc_09=mytraindata$ps_calc_09*7
mytraindata$ps_calc_05=mytraindata$ps_calc_05*6
mytraindata$ps_calc_04=mytraindata$ps_calc_04*6
mytraindata$ps_calc_12=mytraindata$ps_calc_12*6
mytraindata$ps_car_12=mytraindata$ps_car_12*6
mytraindata$ps_ind_05_cat=mytraindata$ps_ind_05_cat*3
mytraindata$ps_car_09_cat=mytraindata$ps_car_09_cat*3
mytraindata$ps_ind_02_cat=mytraindata$ps_ind_02_cat*3
mytraindata$ps_car_11=mytraindata$ps_car_11*3
mytraindata$ps_car_04_cat=mytraindata$ps_car_04_cat*3


mytestdata$ps_car_13=mytestdata$ps_car_13*17
mytestdata$ps_car_06_cat=mytestdata$ps_car_06_cat*14
mytestdata$ps_car_14=mytestdata$ps_car_14*12
mytestdata$ps_calc_10=mytestdata$ps_calc_10*10
mytestdata$ps_car_01_cat=mytestdata$ps_car_01_cat*10
mytestdata$ps_ind_15=mytestdata$ps_ind_15*10
mytestdata$ps_calc_14=mytestdata$ps_calc_14*10
mytestdata$ps_ind_03=mytestdata$ps_ind_03*9
mytestdata$ps_calc_11=mytestdata$ps_calc_11*9
mytestdata$ps_reg_02=mytestdata$ps_reg_02*9
mytestdata$ps_calc_01=mytestdata$ps_calc_01*9
mytestdata$ps_calc_03=mytestdata$ps_calc_03*9
mytestdata$ps_calc_02=mytestdata$ps_calc_02*9
mytestdata$ps_calc_13=mytestdata$ps_calc_13*8
mytestdata$ps_car_15=mytestdata$ps_car_15*8
mytestdata$ps_calc_08=mytestdata$ps_calc_08*7
mytestdata$ps_calc_07=mytestdata$ps_calc_07*7
mytestdata$ps_reg_01=mytestdata$ps_reg_01*7
mytestdata$ps_calc_06=mytestdata$ps_calc_06*7
mytestdata$ps_ind_01=mytestdata$ps_ind_01*7
mytestdata$ps_calc_09=mytestdata$ps_calc_09*7
mytestdata$ps_calc_05=mytestdata$ps_calc_05*6
mytestdata$ps_calc_04=mytestdata$ps_calc_04*6
mytestdata$ps_calc_12=mytestdata$ps_calc_12*6
mytestdata$ps_car_12=mytestdata$ps_car_12*6
mytestdata$ps_ind_05_cat=mytestdata$ps_ind_05_cat*3
mytestdata$ps_car_09_cat=mytestdata$ps_car_09_cat*3
mytestdata$ps_ind_02_cat=mytestdata$ps_ind_02_cat*3
mytestdata$ps_car_11=mytestdata$ps_car_11*3
mytestdata$ps_car_04_cat=mytestdata$ps_car_04_cat*3



kmean = kmeans(mytraindata[, 3:59], 200, algorithm = "Lloyd", iter.max = 1000)
error = rep(0, times = 200)
numbe = rep(0, times = 200)
for (j in 1:length(kmean$cluster)) {
    error[kmean$cluster[j]] = error[kmean$cluster[j]] + mytraindata[j, 2]
    numbe[kmean$cluster[j]] = numbe[kmean$cluster[j]] + 1
}
for (j in 1:200)
    error[j] = error[j] / numbe[j]


#knn.pred = knn(mytraindata[, 3:59], mytestdata[1:100,2:58], kmean$cluster, 1)

dis = function (p1, p2) {
    ans = 0
    for (i in 1:length(p1))
        ans = ans + (p1[i] - p2[i]) ^ 2
    return (sqrt(ans))
}

predict = matrix(nrow = nrow(mytestdata), ncol = 2, byrow = T)
#for (i in 1:nrow(mytestdata)) {
for (i in 1:nrow(mytestdata)) {
    weightMin = 0xffffff
    index = 0
    for (j in 1:nrow(kmean$centers)) {
        diste = dis(as.numeric(kmean$centers[j, 1:57]), as.numeric(mytestdata[i, 2:58]))
        if (diste < weightMin) {
            weightMin = diste
            index = j
        }
    }
    print(i)
    predict[i, 2] = error[index]
    predict[i, 1] = mytestdata[i, 1]
    #print(predict[i, 2])
}

myresult<-read.csv(file="../project/sample_submission.csv",header=TRUE,sep=",")
myresult[,2] <- predict[,2]
write.csv(myresult, file = "../project/myresult.csv",row.names = F)
#write.csv(predict, file = "../project/myresult.csv",row.names = F)