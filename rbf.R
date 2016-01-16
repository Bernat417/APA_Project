library(ggplot2)
library(gridExtra)
library(MASS)
library(class)
library(e1071) 
library(cclust)

#Read dataset

set.seed(1234)

whiteWine <- read.table(file = "winequality-white.csv", sep = ";", header = TRUE)
redWine <- read.table(file = "winequality-red.csv", sep = ";", header = TRUE)

learn <- sample(1:nrow(whiteWine), nrow(whiteWine)/10)
whiteWine.learn <- whiteWine[learn,]
whiteWine.test <- whiteWine[-learn,]

whiteWine.learn.input <- whiteWine.learn[,1:11]
whiteWine.learn.classes <- as.factor(whiteWine.learn[,12])

whiteWine.test.input <- whiteWine.test[,1:11]
whiteWine.test.classes <- as.factor(whiteWine.test[,12])




#Funciones del RBF

CalculatePhi <- function(x,m,h) {
  N <- nrow(x)
  M <- length(m)
  phi <- matrix(rep(0,(M)*N), nrow=M, ncol=N)
  for(i in seq(1,M))
  {
    phi[i,] <- exp(-(x - m[i])^2/(h[i]))
  }
  return( t(phi) )
}

CalculateH <- function(x,m) {
  h <- c()
  M <- length(m)
  for (i in seq(1,M)) {
    d.mitjana = mean(abs(m[i]-x))/2
    h <- c(h,d.mitjana)
  }
  return(h)
}


# Calculo de la RBF

bestError <- 999
bestM <- 2
bestW <- c()
bestH <- c()
bestMi <- c()
maxM <- 15
M <- 5
for(M in seq(2,maxM)) {
  
  #Calcular centros, h, phi
  data  <- cbind(whiteWine.learn.input,rep(0,nrow(whiteWine.learn.input))) 
  kmeans <- matrix (nrow=10, ncol=M)
  for(i in seq(1:10)) {
    aux <- cclust(x=as.matrix(whiteWine.learn.input),centers=M,iter.max=200,method="kmeans",dist="euclidean")
    kmeans[i,] <- aux$centers[,1]
  }
  
  m <- c()
  for(i in seq(1:M)) {
    m <- c(m, sum((kmeans[,i])/10.0))
  }  
  
  h <- CalculateH(as.matrix(whiteWine.learn.input),m)
  phi <- CalculatePhi(as.matrix(whiteWine.learn.input),m,h)
 
  #modelo
  model <- lm(whiteWine.learn.classes ~ phi)
  
  #Calcular lambda
  lambdas <- 10^seq(-8,2,0.1)
  fit <- lm.ridge(model, lambda = lambdas)
  g <- glance(fit)
  w <- setNames(coef(lm.ridge(model, lambda=g$lambdaGCV)), paste0("w_", 0:M))
  
  y <- phi %*% w
  errorM <- sum(y != whiteWine.learn.classes)/length(whiteWine.learn.classes)
  if(errorM < bestError) {
    bestM <- M
    bestW <- w
    bestH <- h
    bestMi <- m
    bestError <- errorM
  }
}

#Predecir y calcular para el test

test.phi <- cbind(rep(1,length(whiteWine.test.input)),CalculatePhi(whiteWine.test.input,bestMi,bestH))
y <- test.phi %*% bestW
(errorM <- sum(y != whiteWine.learn.classes)/length(whiteWine.learn.classes))


