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

set.seed(1234)
table(whiteWine$quality)

whiteWine <- read.table(file = "winequality-white.csv", sep = ";", header = TRUE)
redWine <- read.table(file = "winequality-red.csv", sep = ";", header = TRUE)

summary(whiteWine)
table(whiteWine$quality)

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

#fixed.acidity. Parace correcto
hist(whiteWine$fixed.acidity)
hist(log(whiteWine$fixed.acidity))
skewness(whiteWine$fixed.acidity)
skewness(log(whiteWine$fixed.acidity))

#volatile.acidity. Parace correcto
hist(whiteWine$volatile.acidity)
hist(log(whiteWine$volatile.acidity))
skewness(whiteWine$volatile.acidity)
skewness(log(whiteWine$volatile.acidity))

#citric.acid. Parece correcto
hist(whiteWine$citric.acid)
hist(log(whiteWine$citric.acid))
skewness(whiteWine$citric.acid)
skewness(log(whiteWine$citric.acid))

#residual.sugar. Mejora al aplicar el log a los datos
hist(whiteWine$residual.sugar)
hist(log(whiteWine$residual.sugar))
skewness(whiteWine$residual.sugar)
skewness(log(whiteWine$residual.sugar))
whiteWine$residual.sugar <- log(whiteWine$residual.sugar) 

#chlorides. Parece correcto
hist(whiteWine$chlorides)
hist(log(whiteWine$chlorides))
skewness(whiteWine$chlorides)
skewness(log(whiteWine$chlorides))

#free.sulfur.dioxide. Parece correcto
hist(whiteWine$free.sulfur.dioxide)
hist(log(whiteWine$free.sulfur.dioxide))
skewness(whiteWine$free.sulfur.dioxide)
skewness(log(whiteWine$free.sulfur.dioxide))

#total.sulfur.dioxide. Parece correcto
hist(whiteWine$total.sulfur.dioxide)
hist(log(whiteWine$total.sulfur.dioxide))
skewness(whiteWine$total.sulfur.dioxide)
skewness(log(whiteWine$total.sulfur.dioxide))

#density. Parece correcto
hist(whiteWine$density)
hist(log(whiteWine$density))
skewness(whiteWine$density)
skewness(log(whiteWine$density))

#pH. Parece correcto
hist(whiteWine$pH)
hist(log(whiteWine$pH))
skewness(whiteWine$pH)
skewness(log(whiteWine$pH))

#sulphates. Mejora al aplicar el log
hist(whiteWine$sulphates)
hist(log(whiteWine$sulphates))
skewness(whiteWine$sulphates)
skewness(log(whiteWine$sulphates))
whiteWine$sulphates <- log(whiteWine$sulphates)

#alcohol
hist(whiteWine$alcohol)
hist(log(whiteWine$alcohol))
skewness(whiteWine$alcohol)
skewness(log(whiteWine$alcohol))
whiteWine$alcohol <- log(whiteWine$alcohol)


badWine <- subset(whiteWine[-12], whiteWine$quality < 6)
badWine$quality <- as.factor("bad")  

normalWine <- subset(whiteWine[-12], whiteWine$quality == 6)
normalWine$quality <- as.factor("normal")

goodWine <- subset(whiteWine[-12], whiteWine$quality > 6)
goodWine$quality <- as.factor("good")

whiteWine.3 <- rbind(badWine, normalWine, goodWine)

#Parece que las clases ahora esta distribuidas de forma mas uniforme
table(whiteWine.3$quality)

#Separamos un set de entramiento y un set de test

learn <- sample(1:nrow(whiteWine.3), nrow(whiteWine.3)/10)
whiteWine.3.learn <- whiteWine.3[learn,]
whiteWine.3.test <- whiteWine.3[-learn,]

whiteWine.3.learn.input <- whiteWine.3.learn[,1:11]
whiteWine.3.learn.classes <- whiteWine.3.learn[,12]

whiteWine.3.test.input <- whiteWine.3.test[,1:11]
whiteWine.3.test.classes <- whiteWine.3.test[,12]

# K-nearest neighbors

neighbours <- c(1:20)
errors <- matrix (nrow=length(neighbours), ncol=2)
colnames(errors) <- c("k","LOOCV error")

for (k in neighbours)
{
  myknn.cv <- knn.cv (whiteWine.3.learn.input, whiteWine.3.learn.classes, k = neighbours[k])
  
  # fill in no. of neighbours and LOO validation error
  errors[k, "k"] <- neighbours[k]
  
  tab <- table(myknn.cv, whiteWine.3.learn.classes)
  errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
}

errors

# todos estan muy cerca pero parece que k=13 es el mejor valor con 0.52

myknn <- knn (whiteWine.3.learn.input, whiteWine.3.test.input, whiteWine.3.learn.classes, k = 13, prob=TRUE) 


tab <- table(myknn, whiteWine.3.test.classes) 
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)


#LDA

wine.lda <- lda(quality ~ ., data = whiteWine.3.learn)

predict(wine.lda, whiteWine.3.learn)$class

predict(wine.lda, whiteWine.3.learn)$posterior

wine.lda.cv <- lda(quality ~ ., data = whiteWine.3.learn, CV=TRUE)

tab <- table(whiteWine.3.learn$quality, wine.lda.cv$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))


wine.qda <- qda(quality ~ ., prior = c(1,1,1)/3, data = whiteWine.3.learn , CV=TRUE) 

tab <- table(whiteWine.3$quality[learn], wine.qda$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))
