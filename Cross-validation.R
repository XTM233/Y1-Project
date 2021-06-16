#Experiments

library("MASS")
library("class")

#load the dataset
#Somehow there is a certain chance of getting an extra column (seems to be ID) added to the dataset, check before running
churn.whole <- read.csv("churn-whole.csv")
churn.te <- churn.whole[1:667,]
churn.tr <- churn.whole[668:3333,]


V <- 10
V.frac <- nrow(churn.tr)/V   
v.fold.lda.res <- numeric(V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(churn.tr),v.test.idx)
  churn.lda <-  lda(Churn~.,data=churn.tr,subset=v.train.idx)
  churn.lda.pred <- predict(churn.lda,churn.tr[v.test.idx,])$posterior[,2]
  err.rate <- sum(churn.tr[v.test.idx,"Churn"] != (churn.lda.pred > 0.5))/length(v.test.idx)
  v.fold.lda.res[i] <- err.rate
}
mean(v.fold.lda.res)
sqrt(var(v.fold.lda.res))/sqrt(V)


# QDA, train only, 10 fold cross validation. 

v.fold.qda.res <- numeric(V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(churn.tr),v.test.idx)
  rip.qda <-  qda(Churn~.,data=churn.tr,subset=v.train.idx)
  rip.qda.pred <- predict(rip.qda,churn.tr[v.test.idx,])$posterior[,2]
  err.rate <- sum(churn.tr[v.test.idx,"Churn"] != (rip.qda.pred > 0.5))/length(v.test.idx)
  v.fold.qda.res[i] <- err.rate
}
mean(v.fold.qda.res)
sqrt(var(v.fold.qda.res))/sqrt(V)

# LR, train only, 10 fold cross validation. 

v.fold.lr.res <- numeric(V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(churn.tr),v.test.idx)
  rip.lr <-  glm(Churn~.,data=churn.tr,subset=v.train.idx,family="binomial")
  rip.lr.pred <- predict(rip.lr,churn.tr[v.test.idx,],type="response")
  err.rate <- sum(churn.tr[v.test.idx,"Churn"] != (rip.lr.pred > 0.5))/length(v.test.idx)
  v.fold.lr.res[i] <- err.rate
}
mean(v.fold.lr.res)
sqrt(var(v.fold.lr.res))/sqrt(V)





# V fold cross validation for k-nn. 
# Note, the basic structure is the same, it is the details of the classifier that have changed

krange <- 2:75
v.fold.knn.mat <- matrix(0,nrow=length(krange),ncol=V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(churn.tr),v.test.idx)
  k.test.res <- numeric(length(krange))
  # loop over k
  for (j in krange){
    my.knn <-  knn(churn.tr[v.train.idx,1:15],churn.tr[v.test.idx,1:15],churn.tr[v.train.idx,16],k=j)
    knn.cl <- as.numeric(my.knn)-1
    knn.prob <- (attributes(knn(churn.tr[v.train.idx,1:15],churn.tr[v.test.idx,1:15],churn.tr[v.train.idx,16],k=k,prob=T)))$prob
    my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
    err.rate <- sum((my.test.pred >= 0.5) != churn.tr$Churn[v.test.idx])/length(v.test.idx) 
    k.test.res[j-1] <- err.rate
  }
  v.fold.knn.mat[,i] <- k.test.res 
}

v.fold.knn.t.mat <- t(v.fold.knn.mat)
colnames(v.fold.knn.t.mat) <- krange
head(v.fold.knn.t.mat)
boxplot(v.fold.knn.t.mat,xlab="krange")


k.medians <- apply(v.fold.knn.mat,1,median)
min(k.medians)
which.min(k.medians)


k.means <- apply(v.fold.knn.mat,1,median)
ks.std.errs <- apply(v.fold.knn.mat,1,function(x) sqrt(var(x)))/sqrt(V)

plot(krange,k.means)

# compare the minimum mean choices

min.ks <- which(k.means == min(k.means))
cbind(krange[min.ks],k.means[min.ks],ks.std.errs[min.ks])

library("e1071")

v.fold.svm.res <- numeric(V)
for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(churn.tr),v.test.idx)
  my.svm = svm(formula = Churn ~ .,
               data = churn.tr[v.train.idx,],
               type = 'C-classification',
               kernel = 'linear')
  my.test.pred= predict(my.svm, newdata = churn.te[v.test.idx,-16])
  err.rate <- sum(churn.tr[v.test.idx,"Churn"] != (my.test.pred))/length(v.test.idx)
  v.fold.svm.res[i] <- err.rate
}
mean(v.fold.svm.res)
sqrt(var(v.fold.svm.res))/sqrt(V)

# compare different methods

methods <- c("lda","qda","lr","3nn","svm-linear")
results <- cbind(v.fold.lda.res,v.fold.qda.res,v.fold.lr.res,v.fold.knn.t.mat[,2],v.fold.svm.res)
colnames(results) <- methods

boxplot(results,xlab="methods")
head(results)

