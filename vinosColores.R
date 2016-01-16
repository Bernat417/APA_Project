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

whiteWine$color <- as.factor("w")
redWine$color <- as.factor('r')

summary(whiteWine)
summary(redWine)

wine <- rbind(whiteWine, redWine)
summary(wine)

#wine <- na.omit(wine)
table(wine$quality)

#Tratar outlayers
obs <- nrow(wine)

quartiles <- c()
for(i in seq(1:11)) {
  quartiles <- c(quartiles,as.numeric(quantile(wine[,i])[4]))
}

iqr <- c()
for(i in seq(1:11)) {
  iqr <- c(iqr, IQR(wine[,i]))
}

for(i in seq(1:obs)) {
  for(j in seq(1:11)) {
    if (wine[i,j] > quartiles[j] + 1.5*iqr[j]) {
      wine[i,j] <- NA
    }
  }
}

wine <- na.omit(wine)

dim(wine)
table(wine$quality)


#fixed.acidity. Parace correcto
hist(wine$fixed.acidity)
hist(log(wine$fixed.acidity))
skewness(wine$fixed.acidity)
skewness(log(wine$fixed.acidity))
kurtosis(wine$fixed.acidity)

#volatile.acidity. Parace correcto
hist(wine$volatile.acidity)
hist(log(wine$volatile.acidity))
skewness(wine$volatile.acidity)
skewness(log(wine$volatile.acidity))
kurtosis(wine$volatile.acidity)

#citric.acid. Parece correcto
hist(wine$citric.acid)
hist(log(wine$citric.acid))
skewness(wine$citric.acid)
skewness(log(wine$citric.acid))
kurtosis(wine$citric.acid)

#residual.sugar. Mejora al aplicar el log a los datos
hist(wine$residual.sugar)
hist(log(wine$residual.sugar))
skewness(wine$residual.sugar)
skewness(log(wine$residual.sugar))
wine$residual.sugar <- log(wine$residual.sugar) 
kurtosis(wine$residual.sugar)

#chlorides. Parece correcto
hist(wine$chlorides)
hist(log(wine$chlorides))
skewness(wine$chlorides)
skewness(log(wine$chlorides))
kurtosis(log(wine$chlorides))

#free.sulfur.dioxide. Parece correcto
hist(wine$free.sulfur.dioxide)
hist(log(wine$free.sulfur.dioxide))
skewness(wine$free.sulfur.dioxide)
skewness(log(wine$free.sulfur.dioxide))
kurtosis(wine$free.sulfur.dioxide)

#total.sulfur.dioxide. Parece correcto
hist(wine$total.sulfur.dioxide)
hist(log(wine$total.sulfur.dioxide))
skewness(wine$total.sulfur.dioxide)
skewness(log(wine$total.sulfur.dioxide))
kurtosis(wine$total.sulfur.dioxide)

#density. Parece correcto
hist(wine$density)
hist(log(wine$density))
skewness(wine$density)
skewness(log(wine$density))
kurtosis(wine$density)

#pH. Parece correcto
hist(wine$pH)
hist(log(wine$pH))
skewness(wine$pH)
skewness(log(wine$pH))
kurtosis(wine$pH)

#sulphates. Mejora al aplicar el log
hist(wine$sulphates)
hist(log(wine$sulphates))
skewness(wine$sulphates)
skewness(log(wine$sulphates))
kurtosis(wine$sulphates)
wine$sulphates <- log(wine$sulphates)

#alcohol
hist(wine$alcohol)
hist(log(wine$alcohol))
skewness(wine$alcohol)
skewness(log(wine$alcohol))
kurtosis(wine$alcohol)
wine$alcohol <- log(wine$alcohol)

#Buscar correlacion entre la calidad y las variables
#Alcohol es la variable que mas correlacion tiene con la calidad, pero es un 0.4, que continua siendo bajo
print("Correlacion variables y calidad")
names <- names(wine)
for(i in seq(1:11)) {
  correlacion <- cor(wine[,i], wine[,12])
  print(paste(names[i], correlacion, sep=" : "))
}

#Separamos un set de entramiento y un set de test

test <- sample(1:nrow(wine), nrow(wine)/10)
wine.learn <- wine[-test,]
wine.test <- wine[test,]

wine.learn.input <- wine.learn[,1:11]
wine.learn.classes <- wine.learn[,12]

wine.test.input <- wine.test[,1:11]
wine.test.classes <- wine.test[,12]


# K-nearest neighbors

neighbours <- c(1:20)
errors <- matrix (nrow=length(neighbours), ncol=2)
colnames(errors) <- c("k","LOOCV error")

for (k in neighbours)
{
  myknn.cv <- knn.cv (wine.learn.input, wine.learn.classes, k = neighbours[k])
  
  # fill in no. of neighbours and LOO validation error
  errors[k, "k"] <- neighbours[k]
  
  tab <- table(myknn.cv, wine.learn.classes)
  errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
}

errors

# todos estan muy cerca pero parece que k=1 es el mejor valor con 0.39

myknn <- knn (wine.learn.input, wine.test.input, wine.learn.classes, k = 1, prob=TRUE) 


tab
(error <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))


#LDA



wine.lda.cv <- lda(quality ~ ., data = wine.learn, CV=TRUE)

tab <- table(wine.learn$quality, wine.lda.cv$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

wine.lda <- lda(quality ~ ., data = wine.learn)

lda.predictions <- predict(wine.lda, wine.test)
tab <- table(wine.test$quality, lda.predictions$class)  
(error.TE <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))
