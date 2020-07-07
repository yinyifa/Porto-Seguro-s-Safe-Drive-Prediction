library(class)

mytraindata <-
    read.csv(file = "/u/siyixian/365/train.csv")
mytraindata <- as.matrix(mytraindata)
for (i in 3:59)
    mytraindata[, i] = mytraindata[, i]/max(mytraindata[, i])
mytestdata <-
    read.csv(file = "/u/siyixian/365/test.csv")
mytestdata <- as.matrix(mytestdata)
for (i in 2:58)
    mytestdata[, i] = mytestdata[, i]/max(mytestdata[, i])

pca = prcomp(mytraindata[, 2:59])
var = pca$sdev^2/sum(pca$sdev^2)
for (i in 2:length(var))
    var[i] = var[i] + var[i - 1]
plot(var, type = "l")

#ans = c()
#for (i in 2:100) {
#    kmean = kmeans(pca$x[, 1:6], i, algorithm = "Lloyd", iter.max = i * 10)
#    error = rep(0, times = i)
#    numbe = rep(0, times = i)
#    for (j in 1:length(kmean$cluster)) {
#        error[kmean$cluster[j]] = error[kmean$cluster[j]] + mytraindata[j, 2]
#        numbe[kmean$cluster[j]] = numbe[kmean$cluster[j]] + 1
#    }
#    for (j in 1:i)
#        error[j] = error[j] / numbe[j]
#    print(error)
#    ans = c(ans, var(error))
#}
#plot(ans, type = "l")

kmean = kmeans(mytraindata[, 2:59], 40, algorithm = "Lloyd", iter.max = 400)
error = rep(0, times = 40)
numbe = rep(0, times = 40)
for (j in 1:length(kmean$cluster)) {
    error[kmean$cluster[j]] = error[kmean$cluster[j]] + mytraindata[j, 2]
    numbe[kmean$cluster[j]] = numbe[kmean$cluster[j]] + 1
}
for (j in 1:40)
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
    weight = c()
    for (j in 1:nrow(kmean$centers)) {
        weight = c(weight, dis(as.numeric(kmean$centers[j, 2:58]), as.numeric(mytestdata[i, 2:58])))
    }
    ans = 0
    for (j in 1:40)
        ans = ans + (weight[j] * error[j])
    s = sum(weight)
    predict[i, 2] = ans / s
    predict[i, 1] = mytestdata[i, 1]
    print(predict[i, 2])
}

write.csv(predict, file = "/u/siyixian/365/myresult_1.csv",row.names = F)
