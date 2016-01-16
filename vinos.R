library(ggplot2)
library(gridExtra)
library(MASS)
library(class)
library(e1071) 
library(nnet)
library(caret)
library(randomForest)

set.seed(1234)

harm <- function (a,b) { 2/(1/a+1/b) }


whiteWine <- read.table(file = "winequality-white.csv", sep = ";", header = TRUE)
redWine <- read.table(file = "winequality-red.csv", sep = ";", header = TRUE)

summary(whiteWine)
summary(redWine)
table(whiteWine$quality)
table(redWine$quality)

#Tratar outlayers
obs <- nrow(whiteWine)

quartiles <- c()
for(i in seq(1:11)) {
  quartiles <- c(quartiles,as.numeric(quantile(whiteWine[,i])[4]))
}

iqr <- c()
for(i in seq(1:11)) {
  iqr <- c(iqr, IQR(whiteWine[,i]))
}

for(i in seq(1:obs)) {
  for(j in seq(1:11)) {
    if (whiteWine[i,j] > quartiles[j] + 1.5*iqr[j]) {
      whiteWine[i,j] <- NA
    }
  }
}

whiteWine <- na.omit(whiteWine)

dim(whiteWine)
table(whiteWine$quality)


#fixed.acidity. Parace correcto

hist(whiteWine$fixed.acidity)
hist(log(whiteWine$fixed.acidity))
skewness(whiteWine$fixed.acidity)
skewness(log(whiteWine$fixed.acidity))
kurtosis(whiteWine$fixed.acidity)

#volatile.acidity. Parace correcto
hist(whiteWine$volatile.acidity)
hist(log(whiteWine$volatile.acidity))
skewness(whiteWine$volatile.acidity)
skewness(log(whiteWine$volatile.acidity))
kurtosis(whiteWine$volatile.acidity)

#citric.acid. Parece correcto
hist(whiteWine$citric.acid)
hist(log(whiteWine$citric.acid))
skewness(whiteWine$citric.acid)
skewness(log(whiteWine$citric.acid))
kurtosis(whiteWine$citric.acid)

#residual.sugar. Mejora al aplicar el log a los datos
hist(whiteWine$residual.sugar)
hist(log(whiteWine$residual.sugar))
skewness(whiteWine$residual.sugar)
skewness(log(whiteWine$residual.sugar))
whiteWine$residual.sugar <- log(whiteWine$residual.sugar) 
kurtosis(whiteWine$residual.sugar)

#chlorides. Parece correcto
hist(whiteWine$chlorides)
hist(log(whiteWine$chlorides))
skewness(whiteWine$chlorides)
skewness(log(whiteWine$chlorides))
kurtosis(log(whiteWine$chlorides))

#free.sulfur.dioxide. Parece correcto
hist(whiteWine$free.sulfur.dioxide)
hist(log(whiteWine$free.sulfur.dioxide))
skewness(whiteWine$free.sulfur.dioxide)
skewness(log(whiteWine$free.sulfur.dioxide))
kurtosis(whiteWine$free.sulfur.dioxide)

#total.sulfur.dioxide. Parece correcto
hist(whiteWine$total.sulfur.dioxide)
hist(log(whiteWine$total.sulfur.dioxide))
skewness(whiteWine$total.sulfur.dioxide)
skewness(log(whiteWine$total.sulfur.dioxide))
kurtosis(whiteWine$total.sulfur.dioxide)

#density. Parece correcto
hist(whiteWine$density)
hist(log(whiteWine$density))
skewness(whiteWine$density)
skewness(log(whiteWine$density))
kurtosis(whiteWine$density)

#pH. Parece correcto
hist(whiteWine$pH)
hist(log(whiteWine$pH))
skewness(whiteWine$pH)
skewness(log(whiteWine$pH))
kurtosis(whiteWine$pH)

#sulphates. Mejora al aplicar el log
hist(whiteWine$sulphates)
hist(log(whiteWine$sulphates))
skewness(whiteWine$sulphates)
skewness(log(whiteWine$sulphates))
kurtosis(whiteWine$sulphates)
whiteWine$sulphates <- log(whiteWine$sulphates)

#alcohol
hist(whiteWine$alcohol)
hist(log(whiteWine$alcohol))
skewness(whiteWine$alcohol)
skewness(log(whiteWine$alcohol))
kurtosis(whiteWine$alcohol)
whiteWine$alcohol <- log(whiteWine$alcohol)

#Buscar correlacion entre la calidad y las variables
#Alcohol es la variable que mas correlacion tiene con la calidad, pero es un 0.4, que continua siendo bajo
print("Correlacion variables y calidad")
names <- names(whiteWine)
for(i in seq(1:11)) {
  correlacion <- cor(whiteWine[,i], whiteWine[,12])
  print(paste(names[i], correlacion, sep=" : "))
}


#Parece que las clases ahora esta distribuidas de forma mas uniforme
table(whiteWine$quality)

#Separamos un set de entramiento y un set de test
whiteWine$quality <- as.factor(whiteWine$quality)

test <- sample(1:nrow(whiteWine), nrow(whiteWine)/10)
whiteWine.learn <- whiteWine[-test,]
whiteWine.test <- whiteWine[test,]

whiteWine.learn.input <- whiteWine.learn[,1:11]
whiteWine.learn.classes <- whiteWine.learn[,12]

whiteWine.test.input <- whiteWine.test[,1:11]
whiteWine.test.classes <- whiteWine.test[,12]

# K-nearest neighbors

neighbours <- c(1:20)
errors <- matrix (nrow=length(neighbours), ncol=2)
colnames(errors) <- c("k","LOOCV error")

for (k in neighbours)
{
  myknn.cv <- knn.cv (whiteWine.learn.input, whiteWine.learn.classes, k = neighbours[k])
  
  # fill in no. of neighbours and LOO validation error
  errors[k, "k"] <- neighbours[k]
  
  tab <- table(myknn.cv, whiteWine.learn.classes)
  errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
}

errors

# todos estan muy cerca pero parece que k=1 es el mejor valor con 0.41

myknn <- knn (whiteWine.learn.input, whiteWine.test.input, whiteWine.learn.classes, k = 1, prob=TRUE) 


tab <- table(myknn, whiteWine.test.classes) 
tab
(error <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))


#LDA

wine.lda <- lda(quality ~ ., data = whiteWine.learn)

predict(wine.lda, whiteWine.learn)$class

predict(wine.lda, whiteWine.learn)$posterior

wine.lda.cv <- lda(quality ~ ., data = whiteWine.learn, CV=TRUE)

tab <- table(whiteWine.learn$quality, wine.lda.cv$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

wine.qda <- qda(quality ~ ., prior = c(1,1,1)/3, data = whiteWine.learn , CV=TRUE) 

tab <- table(whiteWine.learn$quality, wine.qda$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))



# Random forest

(ntrees <- round(10^seq(1,3,by=0.2)))


rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0

ii <- 1

for (nt in ntrees)
{ 
  print(nt)
  
  model.rf <- randomForest(quality ~ ., data = whiteWine.learn, ntree=nt, proximity=FALSE) 
                           
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  
  ii <- ii+1
}

rf.results



lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])


model.rf <- randomForest(quality ~ ., data = whiteWine.learn, ntree=ntrees.best, proximity=FALSE) 
                         

pred.rf.final <- predict (model.rf, whiteWine.test, type="class")

(ct <- table(Truth=whiteWine.test$quality, Pred=pred.rf.final))


prop.table(ct, 1)

(acierto <- sum(diag(ct))/sum(ct))


(per.error <- round(100*(1-sum(diag(ct))/sum(ct)),2))

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

#MLP

for(i in seq(1:11)) {
  whiteWine.learn[,i] <- scale(whiteWine.learn[,i])
  whiteWine.test[,i] <- scale(whiteWine.test[,i])
}

trc <- trainControl (method="repeatedcv", number=10, repeats=10)
decays <- 10^seq(-3,0,by=1)

model.nnet <- train (quality ~., data = whiteWine.learn, method='nnet', maxit = 200, trace = FALSE,
                     tuneGrid = expand.grid(.size=10,.decay=decays), trControl=trc)

## Take your time to understand the output
model.nnet
model.nnet$value
model.nnet$fitted.values
model.nnet$wts
summary(model.nnet)


p1 <- as.factor(predict (model.nnet, type="class"))

t1 <- table(p1,whiteWine.learn$quality)
error_rate.learn <- 100*(1-sum(diag(t1))/nrow(whiteWine.learn))
error_rate.learn

p2 <- as.factor(predict (model.nnet, newdata=whiteWine.test, type="class"))

t2 <- table(pred=p2,truth=whiteWine.test$quality)
error_rate.test <- 100*(1-sum(diag(t2))/nrow(whiteWine.test))
error_rate.test

t2 <- table(pred=p2,truth=Admis$admit[-learn])
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test


