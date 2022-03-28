##################################################
##################################################
#                  Exercício 1                   #
##################################################
##################################################

##################################################
#### alínea 1.

# variável: totais mensais, em mm, da precipitação
# classificação: quantitativa contínua


##################################################
#### alínea 2.

# O R tem algumas constantes pré-definidas, como por exemplo:
# pi
# abecedário: letters ou LETTERS
# meses do ano: month.name ou month.abb

(amostra <- data.frame(m=month.abb, 
                       p=c(101, 60.7, 75.1, 19.9, 26.7, 10.5,
                          2.5, 39.8, 5.7, 51.7, 50.1,170.6)))

# podemos utilizar a função scan() 
# em vez de escrever os dados basta copiar a informação do pdf

##################################################
#### alínea 3.

# precipitação total
sum(amostra$p)

# medidas de localização central: média, mediana e moda
# medidas de localização não central: quantis -> quartis

# média -> mean()
mean(amostra$p)

# verificar se os dados têm moda
# range() -> vetor com duas posições, na 1ª está o mínimo e na 2ª está o máximo

range(table(amostra$p))  #mínimo e máximo das frequências absolutas

if( range(table(amostra$p))[1]==range(table(amostra$p))[2]){
  print("amodal")
  } else{
  print(paste("Moda:",DescTools::Mode(amostra$p)))
}


# mediana -> meadian()
median(amostra$p)
#ou
# mediana = 1º quartil = quantil 0.50 -> quantile()
quantile(amostra$p, 0.50)

# extremos e quartis
quantile(amostra$p)

# extremos, quartis e média
summary(amostra$p)


# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

# variância de uma amostra -> var()
var(amostra$p)

# desvio padrão de uma amostra -> sd()
sd(amostra$p)

# extremos: mínimo -> min() ;  máximo -> max()
min(amostra$p)
max(amostra$p)

# amplitude total = A = máx-min
(A = max(amostra$p)-min(amostra$p))
#ou
(A = range(amostra$p)[2]-range(amostra$p)[1])

# amplitude interquartil = AIQ = Q3-Q1
(AIQ = quantile(amostra$p, 0.75)-quantile(amostra$p, 0.25))
#ou
(AIQ = IQR(amostra$p))


##################################################
#### alínea 4.

# which()

# (a)
amostra[which(amostra$p==min(amostra$p)),1]
amostra[which(amostra$p==min(amostra$p)),]

# (b)
amostra[which(amostra$p==max(amostra$p)),1]

# (c)
amostra[which(amostra$p>mean(amostra$p)),1]

##################################################
#### alínea 5.

(amostra.meses <- amostra[6:9,])
#ou
(amostra.meses=amostra[which(amostra$m %in% c("Jun","Jul","Aug","Sep")),])

# medidas de localização
summary(amostra.meses$p)

if( range(table(amostra.meses$p))[1]==range(table(amostra.meses$p))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(amostra.meses$p)))
}


##################################################
#### alínea 6.

amostra[which(amostra$p>50),1]


##################################################
#### alínea 7.

# tabela de frequências 
# como os dados são quantitativos contínuos -> classes

# Regra Sturges
# k = número de classes = número de linhas da tabela
# k = parte inteira ( 1+log(n)/log(2))

# n = dimensão da amostra
(n <- length(amostra$p))
dim(amostra)[1]

(k<-trunc(1+log(n)/log(2)))

# amplitude de cada classe = h
(A = range(amostra$p)[2]-range(amostra$p)[1])
(h<-A/k)

#limites das classes
(cortes <- seq(min(amostra$p), max(amostra$p), by=h))
# só é válido se não arredondar o h, caso contrário tem de ajustar o máximo

# com h arredondado é necessário ajustar limite máximo das classes
(maxclasse <- max(min(amostra$p)+k*h,max(amostra$p)))
(cortes <- seq(min(amostra$p), maxclasse, by=h))

#definir as classes -> cut()
# classes fechadas à direita -> ] , ] = ( , ]
# classes fechadas à esquerda -> [ , [ = [ , )

(classes <- cut(amostra$p, breaks=cortes, right=TRUE, 
                include.lowest=TRUE))
levels(classes)
  
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
                       fi=round(as.numeric(fi),3),
                       Ni=as.integer(Ni),
                       Fi=round(as.numeric(Fi),3)))

#classe modal
if(range(tab.frq$ni)[1]==range(tab.frq$ni)[2]){
  print("amodal")
} else{
  print(tab.frq[which(tab.frq$ni==max(tab.frq$ni)),1])
}

#####################################################

# para gráficos mais "bonitos" ver: library -> ggplot2

#####################################################

# gráfico -> histograma -> hist()

# histograma -> eixo dos yy -> frequências absolutas
hist(amostra$p, breaks=cortes,
     main="histograma",
     xlab="precipitação",
     col=2,
     xlim=c(0,200))

# histograma -> eixo dos yy -> fi/h
hist(amostra$p, breaks=cortes, freq=FALSE,
     main="histograma",
     xlab="precipitação",
     col=2,
     xlim=c(0,200))

# histograma -> eixo dos yy -> colocar o que se pretende, por exemplo, fi
graf <- hist(amostra$p, breaks=cortes, freq=FALSE)
graf$density <- fi
plot(graf, freq=FALSE,
     main="histograma",
     xlab="precipitação",
     ylab="frequências relativas",
     col=2,
     xlim=c(0,200))

##################################################
#### alínea 8.

# amplitude de cada classe = h
(h2<-30)

#limites das classes
(cortes2 <- seq(0, 180, by=h2))

#definir as classes -> cut()
# classes fechadas à direita -> ] , ] = ( , ]
# classes fechadas à esquerda -> [ , [ = [ , )

(classes2 <- cut(amostra$p, breaks=cortes2, right=TRUE, 
                include.lowest=TRUE))
levels(classes2)

# frequências absolutas
(ni2<-table(classes2))
# nomes das classes
(nclasses2 <- rownames(ni2))
# frequências relativas
(fi2<-ni2/n)
#frequências absolutas acumuladas
(Ni2<-cumsum(ni2))
#frequências relativas acumuladas
(Fi2<-Ni2/n)

(tab.frq2 <- data.frame(classes = nclasses2,
                       ni=as.integer(ni2),
                       fi=round(as.numeric(fi2),3),
                       Ni=as.integer(Ni2),
                       Fi=round(as.numeric(Fi2),3)))

#classe modal
if(range(tab.frq2$ni)[1]==range(tab.frq2$ni)[2]){
  print("amodal")
} else{
  print(tab.frq2[which(tab.frq2$ni==max(tab.frq2$ni)),1])
}

#####################################################

# para gráficos mais "bonitos" ver: library -> ggplot2

#####################################################

# gráfico -> histograma -> hist()

# histograma -> eixo dos yy -> frequências absolutas
hist(amostra$p, breaks=cortes2,
     main="histograma",
     xlab="precipitação",
     col=2,
     xlim=c(0,200))

# histograma -> eixo dos yy -> fi/h
hist(amostra$p, breaks=cortes2, freq=FALSE,
     main="histograma",
     xlab="precipitação",
     col=2,
     xlim=c(0,200))

# histograma -> eixo dos yy -> colocar o que se pretende, por exemplo, fi
graf2 <- hist(amostra$p, breaks=cortes2, freq=FALSE)
graf2$density <- fi2
plot(graf2, freq=FALSE,
     main="histograma",
     xlab="precipitação",
     ylab="frequências relativas",
     col=2,
     xlim=c(0,200))


##################################################
##################################################
#                  Exercício 2                   #
##################################################
##################################################

data() # ver as diversas base de dados existentes no R

sunspots
help(sunspots)
class(sunspots)

#serie temporal -> ts -> vetor com a indicação de datas de ocorrência dos valores
plot(sunspots)

##################################################
#### alínea 1.

# variável: média de manchas solares observadas por mês durante os anos de 1749 a 1983
# classificação: quantitativa contínua


##################################################
#### alínea 2.

# tabela de frequências 
# como os dados são quantitativos contínuos -> classes

# Regra Sturges
# n = dimensão da amostra
(n <- length(sunspots))
# k = número de classes = número de linhas da tabela
(k<-trunc(1+log(n)/log(2)))

# amplitude de cada classe = h
(A = range(sunspots)[2]-range(sunspots)[1])
(h<-A/k)

#limites das classes
(cortes <- seq(min(sunspots), max(sunspots), by=h))

#definir as classes -> cut()
# classes fechadas à direita -> ] , ] = ( , ]
(classes <- cut(sunspots, breaks=cortes, right=TRUE, 
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

#####################################################

# para gráficos mais "bonitos" ver: library -> ggplot2

#####################################################

# gráfico -> histograma -> hist()

# histograma -> eixo dos yy -> frequências absolutas
hist(sunspots, breaks=cortes,
     main="histograma",
     xlab="sunspots",
     ylab="frequências absolutas",
     col="gold",
     xlim=c(0,300))



##################################################
#### alínea 3.

# amplitude de cada classe = h
(h2<-10)

#limites das classes
(cortes2 <- seq(0, 260, by=h2))

#definir as classes -> cut()
# classes fechadas à direita -> ] , ] = ( , ]
(classes2 <- cut(sunspots, breaks=cortes2, right=TRUE, 
                 include.lowest=TRUE))

# frequências absolutas
(ni2<-table(classes2))
# nomes das classes
(nclasses2 <- rownames(ni2))
# frequências relativas
(fi2<-ni2/n)
#frequências absolutas acumuladas
(Ni2<-cumsum(ni2))
#frequências relativas acumuladas
(Fi2<-Ni2/n)

(tab.frq2 <- data.frame(classes = nclasses2,
                        ni=as.integer(ni2),
                        fi=round(as.numeric(fi2),3),
                        Ni=as.integer(Ni2),
                        Fi=round(as.numeric(Fi2),3)))


#####################################################

# para gráficos mais "bonitos" ver: library -> ggplot2

#####################################################

# gráfico -> histograma -> hist()

# histograma -> eixo dos yy -> frequências absolutas
hist(sunspots, breaks=cortes2,
     main="histograma",
     xlab="sunspots",
     ylab="frequências absolutas",
     col="yellow",
     xlim=c(0,300))


##################################################
#### alínea 4.

#extremos
min(sunspots)
max(sunspots)
range(sunspots)

#quartis
quantile(sunspots, c(0.25,0.50,0.75))

#decil
#D9=Q0.90
quantile(sunspots, 0.90)

#percentil
#P3=Q0.03
quantile(sunspots, 0.03)


# medidas de localização central: média, mediana e moda

#moda
if( range(table(sunspots))[1]==range(table(sunspots))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(sunspots)))
}

#classe modal
if(range(tab.frq$ni)[1]==range(tab.frq$ni)[2]){
  print("amodal")
} else{
  print(tab.frq[which(tab.frq$ni==max(tab.frq$ni)),1])
}

# média -> mean()
mean(sunspots)

# mediana = 1º quartil = quantil 0.50 -> quantile()
quantile(sunspots, 0.50)


# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

# variância de uma amostra -> var()
var(sunspots)

# desvio padrão de uma amostra -> sd()
sd(sunspots)

# amplitude total = A = máx-min
(A = max(sunspots)-min(sunspots))


# amplitude interquartil = AIQ = Q3-Q1
(AIQ = IQR(sunspots))


# medidas de assimetria e curtose ou achatamento
library(e1071)
# assimetria -> skewness() -> b1
# curtose ou achatamento -> kurtosis() -> b2

# assimetria
skewness(sunspots, type=3)

# curtose ou achatamento
#ATENÇÃO: a interpretação é feita comparando com uma distribuição simétrica (curva da distribuição Normal)
kurtosis(sunspots, type=3)


##################################################
#### alínea 5.

# Diagrama de extremos e quartis = caixa com bigodes -> boxplot()

boxplot(sunspots)

#sem indicação de outliers
boxplot(sunspots, col="gold", main="Diagrama de extremos e quartis", range=0, horizontal=TRUE, xlab="sunspots")

# outliers a partir dos moderados
boxplot(sunspots, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="sunspots")

#ver quem são os outliers
ver.outliers <-boxplot(sunspots, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="sunspots")
ver.outliers$out
length(ver.outliers$out)

# outliers a partir dos severos
boxplot(sunspots, col="gold", main="Diagrama de extremos e quartis", range=3, horizontal=TRUE, xlab="sunspots")


##################################################
##################################################
# comparar o ano 1749 com o 1983

# 1749 -> primeiros 12 valores
# 1983 -> últimos 12 valores

(anos <- data.frame(A1749=sunspots[1:12],
                   A1983=sunspots[(length(sunspots)-11):length(sunspots)]))

boxplot(anos)




