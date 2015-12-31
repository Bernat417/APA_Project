#Read datasets
whiteWine <- read.table(file = "winequality-white.csv", sep = ";", header = TRUE)
redWine <- read.table(file = "winequality-red.csv", sep = ";", header = TRUE)

na.omit(whiteWine)
na.omit(redWine)



#Comprobar columnas a 0 en whiteWine

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




