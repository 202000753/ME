## PREPARING THE WORKSPACE ##

## OBS: CHANGE PATH##
setwd("C:/Users/emaba/Desktop/Projeto ME 2022/projeto1/Segunda fase")
getwd()


## READING DATASET ##
library(readxl)
dfMain<-read_excel("Korea Income and Welfare.xlsx") 


## GENERAL SAMPLE INFO ##

##dfMain general data info##
dim(dfMain)## 92857    14
str(dfMain)
summary(dfMain)## NA's   :33643 +   NA's   :33642  + NA's   :60710  
head(dfMain)
tail(dfMain)


##CLEANING THE SAMPLE DATA##

###VARIABLE occupation(REPLACE THE 'NA' WITH 0, WE HAVE NO KNOWLEDGE OF WHAT THE OCCUPATION OF THE SUBJECT OF THIS OBSERVATION IS)
dfMain$occupation[is.na(dfMain$occupation)]<-0
range(dfMain$occupation)
unique(dfMain$occupation)
dfMain$occupation[dfMain$occupation == 9999]<-999 ##remove outlier
range(dfMain$occupation)

##VARIABLE company_size(REPLACE THE 'NA' WITH 0, WE HAVE NO KNOWLEDGE OF WHAT THE COMPANY SIZE OF THE SUBJECT OF THIS OBSERVATION IS)
dfMain$company_size[is.na(dfMain$company_size)]<-0
dfMain$company_size[dfMain$company_size >= 20]<-0
range(dfMain$company_size)
unique(dfMain$company_size)

##VARIABLE reason_none_worker(REPLACE THE 'NA' and the '99' WITH 0, WE HAVE NO KNOWLEDGE OF WHAT THE NONE WORKER REASON OF THE SUBJECT OF THIS OBSERVATION IS)  
dfMain$reason_none_worker[is.na(dfMain$reason_none_worker)]<-0
dfMain$reason_none_worker[dfMain$reason_none_worker == 99]<-0
range(dfMain$reason_none_worker)
unique(dfMain$reason_none_worker)

dim(dfMain) 

## CHARACTERIZATION OF THE SELECTED VARIABLES


##VARIABLE year  


#FREQUENCY TABLE(TABELA DE FREQUENCIAS)
(ni <- table(dfMain$year)) #frequências absolutas

(xi <- rownames(ni)) #níveis da variável

(n <- sum(ni))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas

(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas


#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)


colours<-rainbow(14) # Cria 14 cores diferentes para serem utilizadas nas representações gráficas

# Gráfico de Barras

barplot(ni, names.arg=xi, ylim=c(0,8000), 
        ylab="Frequências Absolutas", xlab="Ano do Estudo",
        main="Gráfico de Barras:
        Ano em que o estudo foi conduzido", 
        col=colours)
box(bty = "L")

boxplot(dfMain$year, range=1.5, col=14)

# Gráfico Circular

pie(ni, labels=paste(xi,"-",round(fi*100), "%"), 
    col=colours,
    main="Gráfico Circular: Ano do Estudo")
legend("topleft", legend=c(xi), 
       fill=colours, cex = 0.55)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: média, mediana e moda

#MODA
if( range(table(dfMain$year))[1]==range(table(dfMain$year))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(dfMain$year)))
}

mean(dfMain$year)#MEDIA

median(dfMain$year)#MEDIANA

#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(dfMain$year, c(0.25,0.50,0.75))#QUARTIS

quantile(dfMain$year, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

var(dfMain$year) #VARIÂNCIA

sd(dfMain$year) #DESVIO PADRÃO

(A = max(dfMain$year)-min(dfMain$year))#AMPLITUDE TOTAL

(AIQ = IQR(dfMain$year))#AMPLITUDE INTERQUARTIL

##Assimetria

library(e1071)
skewness(dfMain$year,type=3)
##Assimetria Negativa* dado que b1<0 (-0.03954851)

## Curtose

kurtosis(dfMain$year,type = 3)
## Curva Platicúrtica* ou achatada dado que b2<0 (-1.182807)


##VARIABLE region   
#### 1) Seoul 2) Kyeong-gi 3) Kyoung-nam 4) Kyoung-buk 5) Chung-nam 6) Gang-won &. Chung-buk 7) Jeolla & Jeju

#FREQUENCY TABLE(TABELA DE FREQUENCIAS)

(ni <- table(dfMain$region)) #frequências absolutas

(xi <- c("Seoul","Kyeong-gi","Kyoung-nam","Kyoung-buk","Chung-nam","Gang-won & Chung-buk","Jeolla & Jeju")) #níveis da variável

(n <- sum(ni))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas

(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi, # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas

#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)

colours<-rainbow(7) # Cria 7 cores diferentes para serem utilizadas nas representações gráficas

# Gráfico de Barras

barplot(ni, names.arg=xi, ylim=c(0,20000), 
        ylab="Frequências Absolutas", xlab="Regiões da Korea em Estudo",
        main="Gráfico de Barras: Regiões", 
        col=colours)
box(bty = "L")


# Gráfico Circular

pie(ni, labels=paste(xi,"-", round(fi*100),"%"), 
    col=colours,
    main="Gráfico Circular: Regiões da Korea em Estudo")
legend("topright", legend=c(xi), 
       fill=colours, cex = 0.55)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: moda

#MODA
if( range(table(dfMain$region))[1]==range(table(dfMain$region))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(dfMain$region)))
}



library(e1071)
skewness(dfMain$region,type=3)
##Assimetria Positiva* dado que b1>0 (0.3701752)

## Curtose

kurtosis(dfMain$region,type = 3)
## Curva Platicúrtica* ou achatada dado que b2<0 (-1.172695)

#caixa de bigodes
boxplot(dfMain$region, range=1.5, col=7)



##VARIABLE income   


#FREQUENCY TABLE(TABELA DE FREQUENCIAS)

incomeValues <- as.numeric(dfMain$income)
dfMain$income<-incomeValues

outliers <- dfMain$income[which(dfMain$income>=10000 | dfMain$income<0)]

summary(outliers)

dfCleanSample<-dfMain
dfCleanSample<- x[-which(dfCleanSample$income %in% outliers),]

summary(dfCleanSample)
range(dfCleanSample)



# Regra Sturges
(n <- length(dfCleanSample$income)) # n = dimensão da amostra

(k<-trunc(1+log(n)/log(2)))# k = número de classes = número de linhas da tabela

# amplitude de cada classe = h
(A = range(dfCleanSample$income)[2]-range(dfCleanSample$income)[1])
(h<-A/k)

max(dfCleanSample$income)

#limites das classes
(cortes <- seq(min(dfCleanSample$income), max(dfCleanSample$income), by=h))

#definir as classes -> cut()
# classes fechadas à direita -> ] , ] = ( , ]
(classes <- cut(dfCleanSample$income, breaks=cortes, right=TRUE,
                include.lowest=TRUE))

# frequências absolutas
(ni<-table(classes))
# nomes das classes
(nclasses <- rownames(ni))
# frequências relativas
(fi<-ni/n)
#frequências absolutas acumuladas
(Ni<-cumsum(ni))
#frequências relativas acumuladas
(Fi<-Ni/n)

(tab.frq <- data.frame(classes = nclasses,
                       ni=as.integer(ni),
                       fi=round(as.numeric(fi),4),
                       Ni=as.integer(Ni),
                       Fi=round(as.numeric(Fi),4)))

#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)

# Histograma

(histCortes <- cortes) 

hist(dfCleanSample$income, breaks=histCortes,
     main="Histograma ",
     xlab="Income",
     ylab="Frequências absolutas",
     col="gold",
     xlim=c(0,12000),  
     ylim=c(0,25000))


## Caixa de Bigodes
boxplot(dfCleanSample$income, range=1.5)


#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: média, mediana e moda

#CLASSE MODAL
if(range(tab.frq$ni)[1]==range(tab.frq$ni)[2]){
  print("amodal")
} else{
  print(tab.frq[which(tab.frq$ni==max(tab.frq$ni)),1])
}

mean(dfCleanSample$income)#MEDIA

median(dfCleanSample$income)#MEDIANA

#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(dfCleanSample$income, c(0.25,0.50,0.75))#QUARTIS

quantile(dfCleanSample$income, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

var(dfCleanSample$income) #VARIÂNCIA

sd(dfCleanSample$income) #DESVIO PADRÃO

(A = max(dfCleanSample$income)-min(dfCleanSample$income))#AMPLITUDE TOTAL

(AIQ = IQR(dfCleanSample$income))#AMPLITUDE INTERQUARTIL

skewness(dfCleanSample$income,type=3)
##Assimetria Positica dado que b1>0 (0.978392)

## Curtose

kurtosis(dfCleanSample$income,type = 3)
#curva leptocúrtica, alongada dado que b2>0(0.18848)

#######################################################
#######################################################
##                Regressão Linear

# variáveis: 
# X - company_size
# Y - year_born

(amostra <- data.frame(x=dfMain$company_size, 
                       y=dfMain$year_born))
##################################################

# analisar a relação linear entre as variáveis:
# diagrama de dispersão -> plot()
# coeficiente de correlação linear de Pearson -> cor()

##################################################

plot(x=amostra$x, y=amostra$y,
     pch=20, 
     xlab="Tamanho da empresa", 
     ylab="Ano de nascimento",
     main="Diagrama de Dispersão")

##################################################


# coeficiente de correlação linear de Pearson -> cor()

cor(x=amostra$x, y=amostra$y)

##Este foi o valor (0.5082532) mais elevado de coeficiente de correlação linear de Pearson(não é apropriado o modelo)
##########################################################################################
##################################################


# pela análise do diagrama de dispersão não se vê-se uma relação linear entre as variáveis
# pois não é possível imaginar uma reta nem com declive negativo nem com declive positivo a passar pela nuvem de pontos


# rxy= 0.5082532  -> o coeficiente confirma o que vimos no diagrama de dispersão, a correlação linear é muito fraca, 
# não se encontra entre -1<rxy<-0.8 nem entre 0.8<rxy<1.
# Seria a melhor opção abandonar este modelo, contudo no âmbito deste trabalho vamos mantê-lo



###########################################################################################
###########################################################################################


# reta de regressão linear  -> lm()
# variável independente -> company_size = X
# variável dependente -> year_born = Y

# modelo pretendido é y^=a+bx
# formula:  y~x

(modelo <- lm(formula = y~x, data=amostra))

modelo$coefficients
(a1 <- modelo$coefficients[[1]])
(b1 <- modelo$coefficients[[2]])

# ver a reta y^=a+bx no diagrama de dispersão
plot(x=amostra$x, y=amostra$y,
     pch=20, col="blue",
     xlab="Tamanho da companhia", 
     ylab="Ano de Nascimento",
     main="Diagrama de Dispersão")
abline(a=a1,b=b1, col="red")

##################################################

##################################################

# resíduos

modelo$residuals

# ou
# resíduos = valores observados - valores estimados = y -y^

# valores observados:
amostra$y

# valores estimados
modelo$fitted.values

# resíduos
(residuos <- amostra$y - modelo$fitted.values)

# gráfio dos resíduos

# na regressão linear simples é indiferente fazer a análise dos resíduos 
# com o diagrama de dispersão entre a variável x e os resíduous
# ou 
# com o diagrama de dispersão entre os valores estimados do y (y^) e os resíduous
# os gráficos não são iguais mas as conclusões são equivalentes

# gráfico dos resíduos -> (x,e)
plot(x=amostra$x, y=modelo$residuals,
     xlab="Tamanho da Companhia",
     ylab="resíduos",
     main="Gráfico dos resíduos")
abline(h=0, col="red")




##################################################
##################################################

#             Previsoes do modelo

##################################################
##################################################



# Ano de nascimento (year_born) =Y -> prever -> variável dependente
# Tamanho da empresa(company_size)=X -> variável independente

# modelo de interesse -> y^=a+bx

predict(modelo, newdata=data.frame(x=c(8,99)))



range(amostra$x)

# pela análise da correlação linear (diagrama e rxy) vimos que o modelo de regressão linear era adequado, logo as previsões 
#efetuadas com o modelo serão de confiança desde que os valores utilizados para x estejam no intervalo
# [1.6, 43] ou não se afaste muito desse intervalo

# logo a previsão para 30 é de confiança
# a previsão para 75 é absurda o que mostra que o modelo não é válido naquela zona 


######################################################
######################################################

#        Analise Por niveis da Variavel Region

######################################################
###################################################### 

tapply(dfMain$company_size,dfMain$region,summary) 

library (plotly)

ggplotneighbourhood_group <- data.frame(Regiao=dfMain$region,
                                        Valor=dfMain$company_size)

##Tamanho da Empresa por Região
fig <- plot_ly(ggplotneighbourhood_group, labels = ~Regiao, values = ~Valor,textinfo='label+percent',type="pie")
fig <- fig %>% layout(title = 'Tamanho da Companhia por Região',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

################################################################################ Region 1) Seoul 

dfFilterRegion1 <- filter(dfMain, dfMain$region  == 1)
summary(dfFilterRegion1)
dim(dfFilterRegion1)
str(dfFilterRegion1)


plot(x=dfFilterRegion1$company_size, y=dfFilterRegion1$year_born,
     pch=20, 
     xlab="Tamanho da empresa", 
     ylab="Ano de nascimento",
     main="Diagrama de Dispersão da Região de Seoul")
abline(a=a1,b=b1, col="red")

# coeficiente de correlação linear de Pearson -> cor()

cor(dfFilterRegion1$company_size, y=dfFilterRegion1$year_born)


# modelo pretendido é y^=a+bx
# formula:  y~x

(modelo1 <- lm(formula = dfFilterRegion1$year_born~dfFilterRegion1$company_size, data=dfFilterRegion1))

modelo1$coefficients
(a1 <- modelo1$coefficients[[1]])
(b1 <- modelo1$coefficients[[2]])

plot(x=dfFilterRegion1$company_size, y=dfFilterRegion1$year_born,
     pch=20, 
     xlab="Tamanho da empresa", 
     ylab="Ano de nascimento",
     main="Diagrama de Dispersão da Região de Seoul")
abline(a=a1,b=b1, col="red")

# resíduos
(residuos1 <- dfFilterRegion1$year_born - modelo1$fitted.values)

# gráfico dos resíduos -> (x,e)
plot(x=dfFilterRegion1$company_size, y=modelo1$residuals,
     xlab="Tamanho da Companhia",
     ylab="resíduos",
     main="Gráfico dos resíduos em Seoul")
abline(h=0, col="red")

## previsões-> são absurdas pois a correlação linear não é forte e o valor 99 encontra se muito longe do intervalo estudado
predict(modelo1, newdata1=data.frame(x=c(8,99)))


################################################################################ Region 2) Kyeong-gi
dfFilterRegion2 <- filter(dfMain, dfMain$region  == 2)
summary(dfFilterRegion2)
dim(dfFilterRegion2)
str(dfFilterRegion2)



cor(dfFilterRegion2$company_size, y=dfFilterRegion2$year_born)



# modelo pretendido é y^=a+bx
# formula:  y~x

(modelo2 <- lm(formula = dfFilterRegion2$year_born~dfFilterRegion2$company_size, data=dfFilterRegion2))

modelo2$coefficients
(a2 <- modelo2$coefficients[[1]])
(b2 <- modelo2$coefficients[[2]])


plot(x=dfFilterRegion2$company_size, y=dfFilterRegion2$year_born,
     pch=20, 
     xlab="Tamanho da empresa", 
     ylab="Ano de nascimento",
     main="Diagrama de Dispersão da Região de Kyeong-gi")

# resíduos
(residuos2 <- dfFilterRegion2$year_born - modelo2$fitted.values)

# gráfico dos resíduos -> (x,e)
plot(x=dfFilterRegion2$company_size, y=modelo2$residuals,
     xlab="Tamanho da Companhia",
     ylab="resíduos",
     main="Gráfico dos resíduos da Região de Kyeong-gi")
abline(h=0, col="red")

## previsões-> são absurdas pois a correlação linear não é forte e o valor 99 encontra se muito longe do intervalo estudado
predict(modelo2, newdata2=data.frame(x=c(8,99)))

################################################################################ Region 3) Kyoung-nam
dfFilterRegion3 <- filter(dfMain, dfMain$region  == 3)
summary(dfFilterRegion3)
dim(dfFilterRegion3)
str(dfFilterRegion3)

plot(x=dfFilterRegion3$company_size, y=dfFilterRegion3$year_born,
     pch=20, 
     xlab="Tamanho da empresa", 
     ylab="Ano de nascimento",
     main="Diagrama de Dispersão da Região de Kyoung-nam")

cor(dfFilterRegion3$company_size, y=dfFilterRegion3$year_born)


# modelo pretendido é y^=a+bx
# formula:  y~x

(modelo3 <- lm(formula = dfFilterRegion3$year_born~dfFilterRegion3$company_size, data=dfFilterRegion3))

modelo3$coefficients
(a3 <- modelo3$coefficients[[1]])
(b3 <- modelo3$coefficients[[2]])

# resíduos
(residuos3 <- dfFilterRegion3$year_born - modelo3$fitted.values)

# gráfico dos resíduos -> (x,e)
plot(x=dfFilterRegion3$company_size, y=modelo3$residuals,
     xlab="Tamanho da Companhia",
     ylab="resíduos",
     main="Gráfico dos resíduos da Região de Região de Kyoung-nam")
abline(h=0, col="red")

## previsões-> são absurdas pois a correlação linear não é forte e o valor 99 encontra se muito longe do intervalo estudado
predict(modelo3, newdata3=data.frame(x=c(8,99)))

################################################################################ Region 4) Kyoung-buk
dfFilterRegion4 <- filter(dfMain, dfMain$region  == 4)
summary(dfFilterRegion4)
dim(dfFilterRegion4)
str(dfFilterRegion4)

plot(x=dfFilterRegion4$company_size, y=dfFilterRegion4$year_born,
     pch=20, 
     xlab="Tamanho da empresa", 
     ylab="Ano de nascimento",
     main="Diagrama de Dispersão da Região de Kyoung-buk")

cor(dfFilterRegion4$company_size, y=dfFilterRegion4$year_born)


# modelo pretendido é y^=a+bx
# formula:  y~x

(modelo4 <- lm(formula = dfFilterRegion4$year_born~dfFilterRegion4$company_size, data=dfFilterRegion4))

modelo4$coefficients
(a4 <- modelo4$coefficients[[1]])
(b4 <- modelo4$coefficients[[2]])

# resíduos
(residuos4 <- dfFilterRegion4$year_born - modelo4$fitted.values)

# gráfico dos resíduos -> (x,e)
plot(x=dfFilterRegion4$company_size, y=modelo4$residuals,
     xlab="Tamanho da Companhia",
     ylab="resíduos",
     main="Gráfico dos resíduos da Região de Região de Kyoung-buk")
abline(h=0, col="red")

## previsões-> são absurdas pois a correlação linear não é forte e o valor 99 encontra se muito longe do intervalo estudado
predict(modelo4, newdata4=data.frame(x=c(8,99)))


################################################################################# Region 5) Chung-nam
dfFilterRegion5 <- filter(dfMain, dfMain$region  == 5)
summary(dfFilterRegion5)
dim(dfFilterRegion5)
str(dfFilterRegion5)

plot(x=dfFilterRegion5$company_size, y=dfFilterRegion5$year_born,
     pch=20, 
     xlab="Tamanho da empresa", 
     ylab="Ano de nascimento",
     main="Diagrama de Dispersão da Região de Chung-nam")

cor(dfFilterRegion5$company_size, y=dfFilterRegion5$year_born)

# modelo pretendido é y^=a+bx
# formula:  y~x

(modelo5 <- lm(formula = dfFilterRegion5$year_born~dfFilterRegion5$company_size, data=dfFilterRegion5))

modelo5$coefficients
(a5 <- modelo5$coefficients[[1]])
(b5 <- modelo5$coefficients[[2]])

# resíduos
(residuos5 <- dfFilterRegion5$year_born - modelo5$fitted.values)

# gráfico dos resíduos -> (x,e)
plot(x=dfFilterRegion5$company_size, y=modelo5$residuals,
     xlab="Tamanho da Companhia",
     ylab="resíduos",
     main="Gráfico dos resíduos da Região de Região de Chung-nam")
abline(h=0, col="red")

## previsões-> são absurdas pois a correlação linear não é forte e o valor 99 encontra se muito longe do intervalo estudado
predict(modelo5, newdata5=data.frame(x=c(8,99)))

################################################################################Region  6) Gang-won &. Chung-buk 
dfFilterRegion6 <- filter(dfMain, dfMain$region  == 6)
summary(dfFilterRegion2)
dim(dfFilterRegion6)
str(dfFilterRegion6)

plot(x=dfFilterRegion6$company_size, y=dfFilterRegion6$year_born,
     pch=20, 
     xlab="Tamanho da empresa", 
     ylab="Ano de nascimento",
     main="Diagrama de Dispersão da Região de Gang-won &. Chung-buk")

cor(dfFilterRegion6$company_size, y=dfFilterRegion6$year_born)



# modelo pretendido é y^=a+bx
# formula:  y~x

(modelo6 <- lm(formula = dfFilterRegion6$year_born~dfFilterRegion6$company_size, data=dfFilterRegion6))

modelo6$coefficients
(a6 <- modelo6$coefficients[[1]])
(b6 <- modelo6$coefficients[[2]])

# resíduos
(residuos6 <- dfFilterRegion6$year_born - modelo6$fitted.values)

# gráfico dos resíduos -> (x,e)
plot(x=dfFilterRegion6$company_size, y=modelo6$residuals,
     xlab="Tamanho da Companhia",
     ylab="resíduos",
     main="Gráfico dos resíduos da Região de Região de 
     Gang-won &. Chung-buk")
abline(h=0, col="red")

## previsões-> são absurdas pois a correlação linear não é forte e o valor 99 encontra se muito longe do intervalo estudado
predict(modelo6, newdata6=data.frame(x=c(8,99)))

################################################################################# Region 7) Jeolla & Jeju
dfFilterRegion7 <- filter(dfMain, dfMain$region  == 7)
summary(dfFilterRegion7)
dim(dfFilterRegion7)
str(dfFilterRegion7)


plot(x=dfFilterRegion7$company_size, y=dfFilterRegion7$year_born,
     pch=20, 
     xlab="Tamanho da empresa", 
     ylab="Ano de nascimento",
     main="Diagrama de Dispersão da Região de Jeolla & Jeju")

cor(dfFilterRegion7$company_size, y=dfFilterRegion7$year_born)


# modelo pretendido é y^=a+bx
# formula:  y~x

(modelo7 <- lm(formula = dfFilterRegion7$year_born~dfFilterRegion7$company_size, data=dfFilterRegion7))

modelo7$coefficients
(a7 <- modelo7$coefficients[[1]])
(b7 <- modelo7$coefficients[[2]])

# resíduos
(residuos7 <- dfFilterRegion7$year_born - modelo7$fitted.values)

# gráfico dos resíduos -> (x,e)
plot(x=dfFilterRegion7$company_size, y=modelo7$residuals,
     xlab="Tamanho da Companhia",
     ylab="resíduos",
     main="Gráfico dos resíduos da Região de Região de 
     Jeolla & Jeju")
abline(h=0, col="red")

## previsões-> são absurdas pois a correlação linear não é forte e o valor 99 encontra se muito longe do intervalo estudado
predict(modelo7, newdata7=data.frame(x=c(8,99)))

