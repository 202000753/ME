# Ficha de Trabalho 3

#Exercicio 1

#1
amostra <- data.frame(x=c(1.6, 9.4, 15.5, 20, 22, 35.5, 43, 40.5, 33),
                      y=c(240, 181, 193, 155, 172, 110, 113, 75, 94))

#2
#a
plot(x=amostra$x, y=amostra$y,
     pch=20,
     xlab="viscosidade do oleo",
     ylab="desgaste do aço macico",
     main="Diagrama de Disperção")

#b
cor(x=amostra$x, y=amostra$y)

cor(x=amostra$x, y=amostra$y, method="pearson")

#4
#a
modelo1 <- lm(formula=y~x, data=amostra)

modelo1$coefficients
a1<-modelo1$coefficients[[1]]
b1<-modelo1$coefficients[[2]]

plot(x=amostra$x, y=amostra$y,
     pch=20,
     xlab="viscosidade do oleo",
     ylab="desgaste do aço macico",
     main="Diagrama de Disperção")
abline(a=a1, b=b1, col="red")

#b
modelo2 <- lm(formula=y~x, data=amostra)

modelo2$coefficients
a2<-modelo2$coefficients[[1]]
b2<-modelo2$coefficients[[2]]

plot(x=amostra$y, y=amostra$x,
     pch=20,
     xlab="viscosidade do oleo",
     ylab="desgaste do aço macico",
     main="Diagrama de Disperção")
abline(a=a2, b=b2, col="red")
  

#5
p.x30<-a1+b1*30

p.x75<-a1+b1*75

#6
#a
modelo1$residuals

residuos<-amostra$y-modelo1$fitted.values

#b
plot(x=amostra$y, y=modelo1$residuals,
     xlab="viscosidade do oleo",
     ylab="desgaste do aço macico",
     main="Diagrama de Disperção")
abline(a=0, b=0, col="red")

  
#Exercicio 2  
#1
library(readr)
fabrica_quimica <- read_csv("C:/Users/nunor/Desktop/ME/Labs/3/fabrica_quimica.txt")
View(fabrica_quimica)

#2
plot(x=fabrica_quimica$Temperatura, y=fabrica_quimica$Vapor,
     xlab="Temperatura",
     ylab="Vapor",
     main="Diagrama de Disperção")

#3
fabrica_quimica[which(fabrica_quimica$Mes=="Mar"), 2] <- 32

#4
cor(x=fabrica_quimica$Temperatura, y=fabrica_quimica$Vapor)

#5


#6


#7


#8
