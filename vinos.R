library(ggplot2)
library(gridExtra)
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

plots <- c()
for (i in 1:11) {
  name = names(whiteWine)[i]
  plot <- ggplot(whiteWine,aes_q(x=as.name(names(whiteWine)[i]))) + 
    geom_histogram(data=subset(whiteWine, whiteWine$quality > 7, select = c(name)),fill = "red", alpha = 0.2) +
    geom_histogram(data=subset(whiteWine, whiteWine$quality < 5,select = c(name)),fill = "blue", alpha = 0.2)  
  plots[[i]] <- plot
}

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




