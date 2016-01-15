library(ggplot2)
library(gridExtra)
library(MASS)
library(class)
library(e1071)
library(nnet)
library(caret)
library(randomForest)

harm <- function (a,b) { 2/(1/a+1/b) }

#Read dataset

set.seed(1234)

whiteWine <- read.table(file = "winequality-white.csv", sep = ";", header = TRUE)
redWine <- read.table(file = "winequality-red.csv", sep = ";", header = TRUE)

test <- sample(1:nrow(whiteWine), nrow(whiteWine)/10)
whiteWine.learn <- whiteWine[-test,]
whiteWine.test <- whiteWine[test,]

#si no lo marcas como NA no hace nada
#na.omit(whiteWine)
#na.omit(redWine)

summary(whiteWine)

top <- head(sort(whiteWine[,6], decreasing = TRUE),10)
top

names(whiteWine)
OutliersB = subset(whiteWine, select = c("free.sulfur.dioxide", "quality" ))
OutliersB = head(OutliersB[order( OutliersB$quality),],10)
OutliersB

c(nrow(subset(whiteWine, whiteWine$quality == 1)),
  nrow(subset(whiteWine, whiteWine$quality == 2)),
  nrow(subset(whiteWine, whiteWine$quality == 3)),
  nrow(subset(whiteWine, whiteWine$quality == 4)),
  nrow(subset(whiteWine, whiteWine$quality == 5)),
  nrow(subset(whiteWine, whiteWine$quality == 6)),
  nrow(subset(whiteWine, whiteWine$quality == 7)),
  nrow(subset(whiteWine, whiteWine$quality == 8)),
  nrow(subset(whiteWine, whiteWine$quality == 9)),
  nrow(subset(whiteWine, whiteWine$quality == 10))  )

head(subset(whiteWine, whiteWine$quality == 9))

summary(subset(whiteWine))[2,1]
summary(subset(whiteWine))[5,1]


wea<-function(low,top) {

  result <- matrix(
       c(0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0),
       nrow=11,
      ncol=10)
  for (i in 1:11)
  {
    topCutoff <- as.numeric(quantile( whiteWine[,i],c(top)))
    lowCutoff <- as.numeric(quantile( whiteWine[,i],c(low)))

    for (j in 1:9)
    {
      data <- subset(whiteWine, whiteWine$quality == j)
      if(nrow(data) > 0) {
        for(k in 1:nrow(data))
        {
          if(data[k,i] > topCutoff){
            result[i,j] <- result[i,j] + 1
          }
          else if(data[k,i] < lowCutoff) {
            result[i,j] <- result[i,j] + 1
          }
        }
        result[i,j] <- result[i,j] / nrow(data)
      }
    }
  }

  return(result)
}



(topCutoff = as.numeric(quantile( whiteWine[,1],c(0.25))))
(lowCutoff = as.numeric(quantile( whiteWine[,1],c(0.75))))
summary(whiteWine[,1])
wea(0.25,0.75)

?quantile


for (i in 1:11) {
  name = names(whiteWine)[i]
  plot <- ggplot(whiteWine,aes_q(x=as.name(names(whiteWine)[i]))) +
    geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c(name)),fill = "red", alpha = 0.2) +
    geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c(name)),fill = "blue", alpha = 0.2)
  plots[[i]] <- plot
}



#SUBSET BEST AND WORT
best = subset(whiteWine, whiteWine$quality > 7)
nrow(best)
best
summary(best)

worst = subset(whiteWine, whiteWine$quality < 5)
nrow(worst)
worst
summary(worst)

mid = subset(whiteWine, whiteWine$quality > 4 & whiteWine$quality < 8)
nrow(mid)
mid
summary(mid)

hist(worst$sulphates)

best[1:180]
cor(best$sulphates[1:180], worst$sulphates[1:180])
cor(best$fixed.acidity[1:180], worst$fixed.acidity[1:180], use = "complete.obs", method = c("pearson", "kendall", "spearman"))
?cor

#Comprobar columnas a 0 en whiteWine
vegLengths <- rbind(best$sulphates[1:180], worst$sulphates[1:180])

names(best)
#now make your lovely plot
require(gridExtra)

plots <- c()
for (i in 1:11) {
  name = names(whiteWine)[i]
  plot <- ggplot(whiteWine,aes_q(x=as.name(names(whiteWine)[i]))) +
    geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c(name)),fill = "red", alpha = 0.2) +
    geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c(name)),fill = "blue", alpha = 0.2)
  plots[[i]] <- plot
}

plotComparative <- function(i) {
  par(mfrow = c(4,3))
  for (j in 1:11) {
    if (i != j) {
      data=subset(whiteWine, whiteWine$quality >= 5 & whiteWine$quality <= 6)
      plot(data[,i], data[,j], col = "yellow", xlim = c(0,max(whiteWine[,i])), ylim = c(0,max(whiteWine[,j])))

      data=subset(whiteWine, whiteWine$quality < 2)
      points(data[,i], data[,j], col = "red")

      data=subset(whiteWine, whiteWine$quality >= 7 & whiteWine$quality <= 8)
      points(data[,i], data[,j], col = "green")

      data=subset(whiteWine, whiteWine$quality >= 3 & whiteWine$quality <= 4)
      points(data[,i], data[,j], col = "orange")

      data=subset(whiteWine, whiteWine$quality >= 9)
      points(data[,i], data[,j], col = "blue")
    }
  }
}

plotComparative(2)

names(data)[3]
xdev.off()
par(mfrow = c(1,1))
data=subset(whiteWine, whiteWine$quality >= 9)
plot(data$fixed.acidity, data$chlorides, col = "blue", xlim = c(0,max(whiteWine$fixed.acidity)), ylim = c(0,max(whiteWine$chlorides)))

data=subset(whiteWine, whiteWine$quality >= 7 & whiteWine$quality <= 8)
points(data$fixed.acidity, data$chlorides, col = "brown")

data=subset(whiteWine, whiteWine$quality >= 5 & whiteWine$quality <= 6)
points(data$fixed.acidity, data$chlorides, col = "green")

data=subset(whiteWine, whiteWine$quality >= 3 & whiteWine$quality <= 4)
points(data$fixed.acidity, data$chlorides, col = "yellow")

data=subset(whiteWine, whiteWine$quality < 2)
points(data$fixed.acidity, data$chlorides, col = "orange")

data=subset(whiteWine, whiteWine$quality < 5)
points(data$fixed.acidity, data$chlorides, col = "red")



plot(data$fixed.acidity, data$chlorides, col = "red", xlim = c(0,max(whiteWine$fixed.acidity)), ylim = c(0,max(whiteWine$chlorides)))
abline(a=0.05,b=0.0)
abline(v=8)
data=subset(whiteWine, whiteWine$quality > 4 & whiteWine$quality < 7)
plot(data$fixed.acidity, data$chlorides, col = "green", xlim = c(0,max(whiteWine$fixed.acidity)), ylim = c(0,max(whiteWine$chlorides)))
abline(a=0.05,b=0.0)
data=subset(whiteWine, whiteWine$quality > 5 & whiteWine$quality < 7)
plot(data$fixed.acidity, data$chlorides, col = "yellow", xlim = c(0,max(whiteWine$fixed.acidity)), ylim = c(0,max(whiteWine$chlorides)))
abline(a=0.05,b=0.0)
abline(v=8)



grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],plots[[8]],plots[[9]],plots[[10]],plots[[11]])


hist(whiteWine[,6])

table(whiteWine[,1] == 0)  # fixed.acidity
table(whiteWine[,2] == 0)  # volatile.acidity
table(whiteWine[,3] == 0)  # citric.acid. 19 filas a 0, outlayers?
table(whiteWine[,4] == 0)  # residual.sugar
table(whiteWine[,5] == 0)  # chlorides
table(whiteWine[,6] == 0)  # free.sulfur.dioxide
table(whiteWine[,7] == 0)  # total.sulfur.dioxide
table(whiteWine[,8] == 0)  # density
table(whiteWine[,9] == 0)  # pH
table(whiteWine[,10] == 0) # sulphates
table(whiteWine[,11] == 0) # alcohol
table(whiteWine[,12] == 0) # quality

summary(whiteWine)

#Comprobar columnas a 0 en redWine

table(redWine[,1] == 0)  # fixed.acidity
table(redWine[,2] == 0)  # volatile.acidity
table(redWine[,3] == 0)  # citric.acid. 132 filas a 0, outlayers?
table(redWine[,4] == 0)  # residual.sugar
table(redWine[,5] == 0)  # chlorides
table(redWine[,6] == 0)  # free.sulfur.dioxide
table(redWine[,7] == 0)  # total.sulfur.dioxide
table(redWine[,8] == 0)  # density
table(redWine[,9] == 0)  # pH
table(redWine[,10] == 0) # sulphates
table(redWine[,11] == 0) # alcohol
table(redWine[,12] == 0) # quality

summary(redWine)

allWine <- rbind(whiteWine,redWine)

attach(whiteWine)

model1 <- glm (quality~., data=whiteWine, family = gaussian)


#Plot las clases comparando 2 a 2 las variables

pairs(whiteWine[,c(1:11)], main = "Chemical components in different wine qualities", col = (1:length(levels(as.factor(whiteWine$quality))))[unclass(as.factor(whiteWine$quality))])

summary(subset(whiteWine, whiteWine[,12] == 1)) #no hay
summary(subset(whiteWine, whiteWine[,12] == 2)) #no hay
summary(subset(whiteWine, whiteWine[,12] == 3))
summary(subset(whiteWine, whiteWine[,12] == 4))
summary(subset(whiteWine, whiteWine[,12] == 5))
summary(subset(whiteWine, whiteWine[,12] == 6))
summary(subset(whiteWine, whiteWine[,12] == 7))
summary(subset(whiteWine, whiteWine[,12] == 8))
summary(subset(whiteWine, whiteWine[,12] == 9))



# Comparatica LDA y QDA

wine.lda <- lda(quality ~ ., data = whiteWine.learn)

predict(wine.lda, whiteWine.learn)$class

predict(wine.lda, whiteWine.learn)$posterior

wine.lda.cv <- lda(quality ~ ., data = whiteWine.learn, CV=TRUE)

tab <- table(whiteWine.learn$quality, wine.lda.cv$class)
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

whiteWine.factor <- data.frame(whiteWine[,1:11],as.factor(whiteWine[,12]))

wine.qda <- qda(quality ~ ., prior = c(1,1,1)/3, data = whiteWine.learn , CV=TRUE)

tab <- table(whiteWine$quality[learn], wine.qda$class)
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

hist(log10(whiteWine$citric.acid))




# K-nearest neighbors

whiteWine.learn.input <- whiteWine.learn[,1:11]
whiteWine.learn.classes <- as.factor(whiteWine.learn[,12])

whiteWine.test.input <- whiteWine.test[,1:11]
whiteWine.test.classes <- as.factor(whiteWine.test[,12])

neighbours <- c(1:10)
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

# todos estan muy cerca pero parece que k=4 es el mejor valor con 0.52

myknn <- knn (whiteWine.learn.input, whiteWine.test.input, whiteWine.learn.classes, k = 4, prob=TRUE)


tab <- table(myknn, whiteWine.test.classes)
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)


#MLP

for(i in seq(1:11)) {
  whiteWine.learn[,i] <- scale(whiteWine.learn[,i])
  whiteWine.test[,i] <- scale(whiteWine.test[,i])
}

whiteWine.learn$quality <- as.factor(whiteWine.learn$quality)
whiteWine.test$quality <- as.factor(whiteWine.test$quality)

model.nnet <- nnet(quality ~., data = whiteWine.learn, size=20, maxit=200, decay=0.01)

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

t2 <- table(p2,whiteWine.test$quality)
error_rate.test <- 100*(1-sum(diag(t2))/nrow(whiteWine.test))
error_rate.test

#Cross validation MLP

#Variando el numero de neuronas
sizes <- 2*seq(1,10,by=1)

trc <- trainControl (method="repeatedcv", number=10, repeats=10)
model.10x10CV <- train (quality ~., data = whiteWine.learn, method='nnet', maxit = 500, trace = FALSE,
                        tuneGrid = expand.grid(.size=sizes,.decay=0), trControl=trc)

model.10x10CV$results

## and the best model found
model.10x10CV$bestTune

#Variando el parametro decay
decays <- 10^seq(-3,0,by=1)

model.10x10CV <- train (quality ~., data = whiteWine.learn, method='nnet', maxit = 500, trace = FALSE,
                        tuneGrid = expand.grid(.size=20,.decay=decays), trControl=trc)

model.10x10CV$results

## and the best model found
model.10x10CV$bestTune


#Random forest


model.rf1 <- randomForest(quality ~ ., data = whiteWine.learn, ntree=100, proximity=FALSE)

model.rf1

pred.rf1 <- predict (model.rf1, whiteWine.test, type="class")

(ct <- table(Truth=whiteWine.test$quality, Pred=pred.rf1))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

#Pesos

model.rf2 <- randomForest(quality ~ ., data = whiteWine.learn, ntree=100, proximity=FALSE,classwt=c(1,2,3,4,5,6))

model.rf2

pred.rf2 <- predict (model.rf2, whiteWine.test, type="class")

(ct <- table(Truth=whiteWine.test$quality, Pred=pred.rf2))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))


#stratify the boosting resamples

model.rf3 <- randomForest(quality ~ ., data = whiteWine.learn, ntree=100, proximity=FALSE,
                          sampsize=c('3' = 1, '4' = 1, '5' = 1, '6' = 1, '7' = 1, '8' = 1), strata=whiteWine.learn$quality)

model.rf3

pred.rf3 <- predict (model.rf3, whiteWine.test, type="class")

(ct <- table(Truth=whiteWine.test$quality, Pred=pred.rf3))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

importance(model.rf3)
