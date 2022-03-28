# Ficha de Trabalho 1 
 
# Exercício 1

# 32 Engenheiros
# 20 Professores
# 16 Analistas
# 12 Alunos

# 1.1. Introduza a amostra no R.

dados <- data.frame(
  tipo = c("engenheiros", "professores", "analistas de dados", "alunos"),
  freqabs = c(32, 20, 16, 12)
)

# 1.2. Recorrendo aos comandos do R, organize os dados numa tabela de frequência, 
# represente-os graficamente e calcule as medidas adequadas.

dados$freqrel<- dados$freqabs/sum(dados$freqabs)*100 #calculo da freq. relativa em % 

library(data.table)
data<- data.table(dados) #cria uma tabela de frequencias
maximo<-max(dados$freqabs) #calculo do maximo das freq. absolutas
moda<-subset(dados$tipo, dados$freqabs==maximo) #calculo da moda

# Representações Gráficas

# Gráfico Circular
pie(dados$freqabs, labels=dados$tipo)

# Gráfico de Barras
barplot(dados$freqabs, xlab="Classes", ylab = "Frequência", names.arg = dados$tipo)

# Exercício 2

# 2.1. Introduza a tabela no R.

# Através do Import Dataset
library(readxl)
obesidade <- read_excel("C:/Users/nunor/Desktop/ME/Labs/1/obesidade.xlsx")
View(obesidade)

# 2.2. Classifique as variáveis.

# Género: Qualitativa Nominal
# Idade: Quantitativa Contínua
# Altura: Quantitativa Contínua
# Peso: Quantitativa Contínua
# FAVC: Qualitativa Nominal
# FCVC: Qualitativa Ordinal
# NPC: Quantitativa Discreta
# CAEC: Qualitativa Ordinal
# Fumar: Qualitativa Nominal
# CH20: Qualitativa Ordinal
# FAF: Qualitativa Ordinal
# CALC: Qualitativa Ordinal
# MTRANS: Qualitativa 

obesidade$FAVC <- factor(obesidade$FAVC, labels=c('Não', 'Sim'))
obesidade$FCVC <- factor(obesidade$FCVC, labels=c('Nunca', 'Às vezes', 'Sempre'))
obesidade$CAEC <- factor(obesidade$CAEC, labels=c('Não', 'Às vezes', 'Frequentemente', 'Sempre'))
obesidade$Fumar <- factor(obesidade$Fumar, labels=c('Não', 'Sim'))
obesidade$CH2O <- factor(obesidade$CH2O, labels=c('Menos de 1L', 'Entre 1/2L', '2L+'))
obesidade$FAF <- factor(obesidade$FAF, labels=c('Não Pratica', '1/2 dias', '3/4 dias', '4+ dias'))
obesidade$CALC <- factor(obesidade$CALC, labels=c('Nunca', 'Às vezes', 'Frequentemente', 'Sempre'))

# 2.3 Considere apenas duas variáveis nominais. Recorrendo aos comandos do R organize 
# os dados em tabelas de frequências, represente-os graficamente e calule as medidas adequadas.

# Considerando as variáveis Género e FAVC

# Genero
obesidade$Genero<-factor(obesidade$Genero)
summary(obesidade$Genero)

table(obesidade$Genero)#Freq Abs
prop.table(table(obesidade$Genero))#Freq Rel
frqtablegenero<-cbind(freqAbs=table(obesidade$Genero), freqRel=prop.table(table(obesidade$Genero)))#Ambas na mesma tabela

# Moda
genero.mo <- names(table(obesidade$Genero))[table(obesidade$Genero)==max(table(obesidade$Genero))]
genero.mo

# Gráfico Circular
piechart(obesidade$Genero, main="Grafico", scale="percent")

# Gráfico de Barras
Barplot(obesidade$Genero, xlab="Género", ylab = "Frequencia", label.bars = "TRUE")

#--------------------------------------------------------------------------------------------------------------------------

# FAVC
obesidade$FAVC<-factor(obesidade$FAVC)
summary(obesidade$FAVC)

table(obesidade$FAVC)#Freq Abs
prop.table(table(obesidade$FAVC))#Freq Rel
frqtablefavc<-cbind(freqAbs=table(obesidade$FAVC), freqRel=prop.table(table(obesidade$FAVC)))#Ambas na mesma tabela

# Moda
FAVC.mo <- names(table(obesidade$FAVC))[table(obesidade$FAVC)==max(table(obesidade$FAVC))]
FAVC.mo

# Gráfico Circular
piechart(obesidade$FAVC, main="Grafico", scale="percent")

# Gráfico de Barras
Barplot(obesidade$FAVC, xlab="Come alimentos calóricos?", ylab = "Frequencia", label.bars = "TRUE")

# 2.4 Considere apenas duas variáveis qualitativas ordinais. Recorrendo aos comandos do R, organize
# os dados em tabelas de frequências, represente-os graficamente e calcule as medidas adequadas.

# Considerando as variáveis FCVC e CAEC

# FCVC
obesidade$FCVC<-factor(obesidade$FCVC)
summary(obesidade$FCVC)

table(obesidade$FCVC)#Freq Abs
prop.table(table(obesidade$FCVC))#Freq Rel
frqtablefavc<-cbind(freqAbs=table(obesidade$FCVC), freqRel=prop.table(table(obesidade$FCVC)))#Ambas na mesma tabela

# Moda
FCVC.mo <- names(table(obesidade$FCVC))[table(obesidade$FCVC)==max(table(obesidade$FCVC))]
FCVC.mo

# Gráfico Circular
piechart(obesidade$FCVC, main="Grafico", scale="percent")

# Gráfico de Barras
Barplot(obesidade$FCVC, xlab="Come vegetais nas refeições?", ylab = "Frequencia", label.bars = "TRUE")

#--------------------------------------------------------------------------------------------------------------------------

# CAEC

obesidade$CAEC<-factor(obesidade$CAEC)
summary(obesidade$CAEC)

table(obesidade$CAEC)#Freq Abs
prop.table(table(obesidade$CAEC))#Freq Rel
frqtablefavc<-cbind(freqAbs=table(obesidade$CAEC), freqRel=prop.table(table(obesidade$FCVC)))#Ambas na mesma tabela

# Moda
CAEC.mo <- names(table(obesidade$CAEC))[table(obesidade$CAEC)==max(table(obesidade$CAEC))]
CAEC.mo

# Gráfico Circular
piechart(obesidade$CAEC, main="Grafico", scale="percent")

# Gráfico de Barras
Barplot(obesidade$CAEC, xlab="Come entre as refeições?", ylab = "Frequencia", label.bars = "TRUE")

# 2.5. Separe os dados por género e volte a analisar as variáveis das alíneas anteriores de modo a
# verificar se existem diferenças nos comportamentos quando os dados são separados por género.

obesidade_feminino<-subset(obesidade,Genero=="Feminino") # Apenas género feminino
obesidade_masculino<-subset(obesidade,Genero=="Masculino") # Apenas género masculino

# Usando a moda da variável CAEC

# Moda de todos os dados
CAEC.mo <- names(table(obesidade$CAEC))[table(obesidade$CAEC)==max(table(obesidade$CAEC))]
CAEC.mo

# Moda da amostra de apenas Género Feminino
CAEC_F.mo <- names(table(obesidade_feminino$CAEC))[table(obesidade_feminino$CAEC)==max(table(obesidade_feminino$CAEC))]
CAEC_F.mo

# Moda da amostra de apenas Género Masculino
CAEC_M.mo <- names(table(obesidade_masculino$CAEC))[table(obesidade_masculino$CAEC)==max(table(obesidade_masculino$CAEC))]
CAEC_M.mo

# Neste caso a moda é sempre igual mas fica na mesma a maneira de o fazer
