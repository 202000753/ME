#1
#1.1
seminario <- data.frame(
  classe = c("engenheiros", "professores", "analistas de dados", "alunos"),
  freqabs = c(32, 20, 16, 12)
)

seminario$freqrel<-seminario$freqabs/sum(seminario$freqabs)*100

#1.2
library(data.table)
data <- data.table(seminario)
maximo <- max(seminario$freqabs)
moda <- subset(seminario$classe, seminario$freqabs==maximo)
barplot(seminario$freqabs, names.agr=seminario$classe)
pie(seminario$freqabs, labels = seminario$classe)

#2
#2.1
library(readxl)
obesidade <- read_excel("C:/Users/nunor/Desktop/ME/Labs/1/obesidade.xlsx")
View(obesidade)

#2.2
summary(obesidade)

summary(obesidade$Idade)
obesidade$FAVC <- factor(obesidade$FAVC, labels = c('Não', 'Sim'))
obesidade$FCVC <- factor(obesidade$FCVC, labels = c('Nunca', 'Às vezes', 'Sempre'))
summary(obesidade$FAVC)
summary(obesidade$FCVC)

#2.3
obesidade$MTRANS <- factor(obesidade$MTRANS)
summary(obesidade$MTRANS)

table(obesidade$MTRANS)
civil.tb <- table(table(obesidade$MTRANS))
prop.table(table(obesidade$MTRANS))
frqtabletrans <- cbind(fa = table(obesidade$MTRANS), fr = prop.table(table(obesidade$MTRANS)))

barplot(obesidade$MTRANS, xlab="CALC", ylab="Frequency", label.bars=TRUE)
barplot(obesidade$MTRANS, xlab="", ylab="", main="Grafico", scale="percent")

civil.mo <- names(civi.tb)[civil.tb == max(civil.tb)]
civil.mo

#2.4
civil.mo <- names(civi.tb)[civil.tb == max(civil.tb)]
civil.mo

#2.5
obesidade_feminino <- subset(obesidade, Genero=='Feminino')
obesidade_masculino <- subset(obesidade, Genero=='Masculino')

mean(as.numeric(obesidade$CH2O))
median(as.numeric(obesidade$CH2O))

mean(as.numeric(obesidade_feminino$CH2O))
median(as.numeric(obesidade_feminino$CH2O))

mean(as.numeric(obesidade_masculino$CH2O))
median(as.numeric(obesidade_masculino$CH2O))