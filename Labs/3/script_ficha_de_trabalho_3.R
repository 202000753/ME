##################################################
##################################################
#                  Exercício 1                   #
##################################################
##################################################

# variáveis: 
# X - viscosidade do óleo
# Y - desgaste do aço macio


(amostra <- data.frame(x=c(1.6, 9.4, 15.5, 20, 22, 35.5, 43, 40.5, 33), 
                       y=c(240, 181, 193, 155, 172, 110, 113, 75, 94)))

# podemos utilizar a função scan() 
# em vez de escrever os dados basta copiar a informação do pdf

##################################################

# analisar a relação linear entre as variáveis:
# diagrama de dispersão -> plot()
# coeficiente de correlação linear de Pearson -> cor()

##################################################

# questão 2 a)
# diagrama de dispersão -> plot()

plot(x=amostra$x, y=amostra$y,
     pch=20, 
     xlab="viscosidade do óleo", 
     ylab="desgaste do aço macio",
     main="Diagrama de Dispersão")

##################################################

# questão 2 b)
# coeficiente de correlação linear de Pearson -> cor()

cor(x=amostra$x, y=amostra$y)
#OU
cor(x=amostra$x, y=amostra$y,method = "pearson")

##################################################

# questão 3)

# pela análise do diagrama de dispersão vê-se uma relação linear negativa entre as variáveis
# pois é possível imaginar uma reta com declive negativo a passar pela nuvem de pontos


# rxy=-0.9377618  -> o coeficiente confirma o que vimos no diagrama de dispersão, a correlação linear
# é negativa pois rxy<0 e forte visto estar muito próximo de -1, ou seja, -1<rxy<-0.8

##################################################
# questão 4)

# reta de regressão linear  -> lm()


#a) variável independente -> viscosidade do óleo = X
#   variável dependente -> desgaste = Y

# modelo pretendido é y^=a+bx

# formula:  y~x

(modelo1 <- lm(formula = y~x, data=amostra))

modelo1$coefficients
(a1 <- modelo1$coefficients[[1]])
(b1 <- modelo1$coefficients[[2]])

# ver a reta y^=a+bx no diagrama de dispersão
plot(x=amostra$x, y=amostra$y,
     pch=20, col="blue",
     xlab="viscosidade do óleo", 
     ylab="desgaste do aço macio",
     main="Diagrama de Dispersão")
abline(a=a1,b=b1, col="red")

##################################################

#b) variável dependente -> viscosidade do óleo = X
#   variável independente -> desgaste = Y

# modelo pretendido é x^=a+by

# formula:  x~y

(modelo2 <- lm(formula = x~y, data=amostra))

modelo2$coefficients
(a2 <- modelo2$coefficients[[1]])
(b2 <- modelo2$coefficients[[2]])

# ver a reta x^=a+by no diagrama de dispersão
plot(x=amostra$y, y=amostra$x,
     pch=20, col="blue",
     ylab="viscosidade do óleo", 
     xlab="desgaste do aço macio",
     main="Diagrama de Dispersão")
abline(a=a2,b=b2, col="red")


##################################################

# questão 5)

# prever o desgaste quando a viscosidade é de 30 e quando é de 75

# desgaste =Y -> prever -> variável dependente
# viscosidade =X -> variável independente

# modelo de interesse -> y^=a+bx

predict(modelo1, newdata=data.frame(x=c(30,75)))

# ou
(p.x30 <- a1+b1*30)
(p.x75 <- a1+b1*75)

range(amostra$x)

# pela análise da correlação linear (diagrama e rxy) vimos que o modelo de regressão linear era adequado, logo as previsões 
#efetuadas com o modelo serão de confiança desde que os valores utilizados para x estejam no intervalo
# [1.6, 43] ou não se afaste muito desse intervalo

# logo a previsão para 30 é de confiança
# a previsão para 75 é absurda o que mostra que o modelo não é válido naquela zona 


##################################################

# questão 6)
# resíduos

modelo1$residuals

# ou
# resíduos = valores observados - valores estimados = y -y^

# valores observados:
amostra$y

# valores estimados
modelo1$fitted.values

# resíduos
(residuos <- amostra$y - modelo1$fitted.values)

# gráfio dos resíduos

# na regressão linear simples é indiferente fazer a análise dos resíduos 
# com o diagrama de dispersão entre a variável x e os resíduous
# ou 
# com o diagrama de dispersão entre os valores estimados do y (y^) e os resíduous
# os gráficos não são iguais mas as conclusões são equivalentes

# gráfico dos resíduos -> (x,e)
plot(x=amostra$x, y=modelo1$residuals,
     xlab="viscosidade",
     ylab="resíduos",
     main="Gráfico dos resíduos")
abline(h=0, col="red")
#ou
abline(a=0,b=0, col="red")

#ou

# gráfico dos resíduos -> (y^,e)
plot(x=modelo1$fitted.values, y=modelo1$residuals,
     xlab="valores estimados",
     ylab="resíduos",
     main="Gráfico dos resíduos")
abline(h=0, col="red")
#ou
abline(a=0,b=0, col="red")

# resíduos devem ser pequenos -> devem estar perto do eixo dos xx
# não devem ter um padrão definido -> devem ser aleatórios


# os resíduos apresentam valores muito afastados do zero e parecem ter um padrão,
# variando entre positivo e negativo

# pode ser uma consequência de ter poucos dados -> uma sugestão é aumentar a dimensão da amostra
# pode significar que embora o modelo de regressão linear simples tenha sido adequado, pode não ser 
# o melhor modelo para explicar y


##################################################
##################################################
#                  Exercício 2                   #
##################################################
##################################################


(dados <- read.table("fabrica_quimica.txt",
                    header=TRUE,
                    dec=","))

##################################################

# questão 2)

# diagrama de dispersão

plot(x=dados$Temperatura, y=dados$Vapor, 
     pch=20,
     xlab="temperatura",
     ylab="vapor",
     main="Diagrama de Dispersão")

#  temos uma observação discordante ou outlier

# pelo diagrama de dispersão parece existir uma correlação linear positiva forte, só o outlier é
# que se afasta 

# coeficiente de correlação linear de Pearson
cor(x=dados$Temperatura, y=dados$Vapor)

# pelo coeficiente de correlação linear de Pearson a correlação linear é negativa e quase nula, ou seja,
# não existe correlação linear entre as variáveis

#  temos uma observação discordante ou outlier -> influente


##################################################

# questão 3)

# corrigir o valor do mês de março

dados[which(dados$Mes=="Mar"),2] <- 32
#ou
dados[3,2] <- 32
dados

##################################################

# questão 4)

# diagrama de dispersão
plot(x=dados$Temperatura, y=dados$Vapor,
     xlab="Temperatura",
     ylab="Vapor de água",
     main="Diagrama de dispersão")

# coeficiente de correlação linear de Pearson
cor(x=dados$Temperatura, y=dados$Vapor)

# com base no diagrama de dispersão e no coeficiente de correlação linear de Pearson,
# a correlação linear entre as variáveis é positiva e próxima de perfeita
# pois rxy>0 e muito próximo de 1 
# e no gráfico a ligação dos pontos é quase uma reta com declive positivo

##################################################

# questão 5)

# modelo de regressão linear simples

# variável independente: X - temperatura
# variável dependente: Y - vapor

# y^=a+bx
(modelo3 <- lm(formula = Vapor~Temperatura, data=dados))

modelo3$coefficients
(a3 <- modelo3$coefficients[[1]])
(b3 <- modelo3$coefficients[[2]])

# ver a reta y^=a+bx no diagrama de dispersão
plot(x=dados$Temperatura, y=dados$Vapor, 
     pch=20, col="blue",
     xlab="temperatura",
     ylab="vapor",
     main="Diagrama de Dispersão")
abline(a=a3,b=b3, col="red")

##################################################

# questão 6)

#resíduos
modelo3$residuals

plot(x=dados$Temperatura, 
     y=modelo3$residuals, 
     pch=20,
     xlab="temperatura",
     ylab="residuos")
abline(h=0, col="red")

# os resíduos são pequenos (próximos de zero) e parecem ser aleatórios 
#(não apresentam uma padrão bem definido)


##################################################

# questão 7)

# Que alteração se espera no volume de vapor de água quando a
# temperatura média mensal aumenta 1ºF?

#por exemplo
# x=30 e x=31
(previsao <- predict(modelo3, newdata=data.frame(Temperatura=c(30,31))))

# quando se aumenta 1 unidade no x o que acontece ao y?
# o y tem como variação o declive da reta.

previsao[2]-previsao[1]

b3


##################################################

# questão 8)

# previsões

predict(modelo3, newdata=data.frame(Temperatura=c(47,19)))

range(dados$Temperatura)


