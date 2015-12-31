library(ggplot2)
#Read datasets
whiteWine <- read.table(file = "winequality-white.csv", sep = ";", header = TRUE)
redWine <- read.table(file = "winequality-red.csv", sep = ";", header = TRUE)

na.omit(whiteWine)
na.omit(redWine)

summary(whiteWine)

top <- head(sort(whiteWine[,6], decreasing = TRUE),10)
top

names(whiteWine)
OutliersB = subset(whiteWine, select = c("free.sulfur.dioxide", "quality" ))
OutliersB = head(OutliersB[order( OutliersB$quality),],10)
OutliersB


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
plot1 <- ggplot(whiteWine,aes(x=fixed.acidity)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("fixed.acidity")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("fixed.acidity")),fill = "blue", alpha = 0.2)  

plot2 <- ggplot(whiteWine,aes(x=volatile.acidity)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("volatile.acidity")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("volatile.acidity")),fill = "blue", alpha = 0.2) 

plot3 <- ggplot(whiteWine,aes(x=citric.acid)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("citric.acid")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("citric.acid")),fill = "blue", alpha = 0.2) 

plot4 <- ggplot(whiteWine,aes(x=residual.sugar)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("residual.sugar")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("residual.sugar")),fill = "blue", alpha = 0.2) 

plot5 <- ggplot(whiteWine,aes(x=chlorides)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("chlorides")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("chlorides")),fill = "blue", alpha = 0.2) 

plot6 <- ggplot(whiteWine,aes(x=free.sulfur.dioxide)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("free.sulfur.dioxide")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("free.sulfur.dioxide")),fill = "blue", alpha = 0.2) 

plot7 <- ggplot(whiteWine,aes(x=total.sulfur.dioxide)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("total.sulfur.dioxide")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("total.sulfur.dioxide")),fill = "blue", alpha = 0.2) 

plot8 <- ggplot(whiteWine,aes(x=density)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("density")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("density")),fill = "blue", alpha = 0.2) 

plot9 <- ggplot(whiteWine,aes(x=pH)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("pH")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("pH")),fill = "blue", alpha = 0.2) 

plot10 <- ggplot(whiteWine,aes(x=sulphates)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("sulphates")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("sulphates")),fill = "blue", alpha = 0.2) 

plot11 <- ggplot(whiteWine,aes(x=alcohol)) + 
  geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c("alcohol")),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c("alcohol")),fill = "blue", alpha = 0.2) 

plots <- c()
for (i in 1:11) {
  aux <- whiteWine[i,]
  name = names(whiteWine)[i]
  plot <- ggplot(whiteWine,aes(x=names)) + 
    geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c(name)),fill = "red", alpha = 0.2) +
    geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c(name)),fill = "blue", alpha = 0.2)  
  plots <- c(plots,plot)
}
plots[1]
grid.arrange(plots[1])

?grid.arrange

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




