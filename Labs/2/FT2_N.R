# Ficha de Trabalho 2

#Exercicio 1
#1
#Variavel: Precipitação durante o ano
#Tipo: Quantitativa Continua

#2
(amostra <- data.frame(m=month.abb,
                       p=c(101.0, 60.7, 75.1, 19.9, 26.7, 10.5, 2.5, 39.8, 5.7, 51.7, 50.1, 170.6)))
view(amostra)

#3
#(a)
sum(amostra$p)

#(b)
mean(amostra$p)

#(c)
median(amostra$p)

#(d)
var(amostra$p)

#(e)
sd(amostra$p)

#(f)
min(amostra$p)

#(g)
max(amostra$p)

#4
#(a)
amostra[which(amostra$p==min(amostra$p)), 1]

#(b)
amostra[which(amostra$p==max(amostra$p)), 1]

#(c)
amostra[which(amostra$p>mean(amostra$p)),1]

#5
(amostra.messes <- amostra[6:9,])
(amostra.messes = amostra[which(amostra$m %in% c("Jun", "Jul", "Aug", "Sep"))])

summary(amostra.messes$p)

#6
amostra[which(amostra$p>50),1]

#7
(n<-length(amostra))
dim(amostra)[1]

(k<-trunc(1+log(n)/log(2)))

(A<-range(amostra$p)[2]-range(amostra$p)[1])
(h<-A/k)

(cortes<-seq(min(amostra$p), max(amostra$p), by=h))

(maxClasse<-max(min(amostra$p)+k*h,max(amostra$p)))
(cortes<-seq(min(amostra$p), maxClasse, by=h))

(classes<-cut(amostra$p, breaks = cortes, right = TRUE, include.lowest = TRUE))


levels(classes)

(ni<-table(classes))
(nClasses<-rownames(ni))
(fi<-ni/n)
(Ni<-cumsum(ni))
(Fi<-Ni/n)

(tab.frq<-data.frame(classes=nClasses, ni=as.integer(ni), fi=round(as.numeric(fi),3), Ni=as.integer(Ni), Fi=round(as.numeric(Fi))))

hist(amostra$p, breaks = cortes, main="Histograma", xlab = "Precipitação", col = 2, xlim = c(0,200))

#8
(h2<-3)
(cortes2<-seq(0, 180, by = 2))
hist(amostra$p, breaks = cortes, main="Histograma", xlab = "Precipitação", col = 2, xlim = c(0,200))


##################################################################################
#Exercicio 2
#1
data()


#2


#3


#4
#(a)


#(b)


#(c)


#(d)


#(e)


#(f)


#(g)

#5
