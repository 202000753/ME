##################################################
##################################################
#                  Exercício 1                   #
##################################################
##################################################

# variável estatística: profissão dos participantes no seminário
# níveis: engenheiro, professor, analista de dados, aluno
# classificação da variável: qualitativa nominal

##################################################

# c() -> vetor
# data.frame() -> é uma matriz que permite ter colunas de tipos diferentes


(amostra <- c(rep("engenheiro",32), 
              rep("professor",20), 
              rep("analista de dados",16), 
              rep("aluno",12)))

##################################################
# tabela de frequências

# table() -> obter as frequências absolutas

# length() -> comprimento de um vetor
# dim() -> dimensão de uma data.frame
# nrow() -> número de linhas de uma data.frame
# ncol() -> número de colunas de uma data.frame


#frequências absolutas
(ni <- table(amostra))

# níveis da variável
(xi <- rownames(ni))

# n = dimensão da amostra
(n <- length(amostra))
#ou
(n <- sum(ni))

#frequências relativas
(fi <- ni/n)
#ou
(fi <- prop.table(ni))

# frequências absolutas acumuladas
(Ni <- cumsum(ni))

#frequências relativas acumuladas
(Fi <- Ni/n)
#ou
(Fi <- cumsum(fi))


# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas


# escrever a tabela num ficheiro txt
write.table(tab.freq, "exer1.txt", quote=FALSE, row.names=F)

# outra possibilidade para fazer a tabela de frequências: 
# library -> DescTools

library(DescTools)
(tab.freq2 <- Freq(amostra))

  
##################################################

# dados qualitativos nominais:
# gráficos de barras -> barplot()
# gráficos circulares -> pie()

# abrir outra janela para gráficos: x11() ou dev.new()
# fechar a janela de gráficos: dev.off()

# gráficos de barras -> barplot()
barplot(ni, names.arg=xi, ylim=c(0,40), 
        ylab="frequências absolutas", xlab="profissões",
        main="Gráfico de Barras", 
        col=c("red", "yellow", "blue", "green"))
box(bty = "L")


# gráficos circulares -> pie()
pie(ni, labels=paste(fi*100, "%"), 
    col=c("red", "yellow", "blue", "green"), 
    main="Gráfico circular")
legend("topright", legend=c(xi), 
       fill=c("red", "yellow", "blue", "green"), cex = 0.7)

# Gráfico Circular 3D
library(plotrix)
pie3D(ni, col=c("red", "yellow", "green", "blue"),labels=paste(fi*100, "%"), main="Gráfico Circular 3D",explode=0.1) 
legend("topright", legend=c(xi), fill=c("red", "yellow", "blue", "green"), ncol=2, cex=0.7, bty="n")

# gráficos lado a lado
par(mfrow=c(1,2)) 
barplot(ni, names.arg=xi, ylim=c(0,40), 
        ylab="frequências absolutas", xlab="profissões",
        main="Gráfico de Barras", 
        col=c("red", "yellow", "blue", "green"))
box(bty = "L")
pie(ni, labels=paste(fi*100, "%"), 
    col=c("red", "yellow", "blue", "green"), 
    main="Gráfico circular")
legend("bottomright", legend=c(xi), 
       fill=c("red", "yellow", "blue", "green"), cex = 0.7)

##################################################

# todo o tipo de gráficos e mais bonitos: library -> ggplot2

##################################################
# medidas: moda -> dados qualitativos

# moda 
tab.freq[which(tab.freq$ni==max(tab.freq$ni)),2]

#ou
DescTools::Mode(amostra)


##################################################
##################################################
#                  Exercício 2                   #
##################################################
##################################################

# ler ficheiros EXCEL -> library -> readxl

# setwd() -> para indicar em que diretoria está o ficheiro ou a trabalhar

library(readxl)
obesidade = read_excel("obesidade.xlsx", sheet=1, col_names=TRUE)

View(obesidade) # ver a tabela

#ou
#no RStudio: File -> Import Dataset

#ver o tipo de dados
str(obesidade)

# ver as primeira linhas
head(obesidade)
# ver as últimas linhas
tail(obesidade)

##################################################
#construir uma função para fazer as tabelas de frequências

tabela.frequencias <- function(x){
  n <- length(x)        # dimensão da amostra
  ni <- table(x)        # frequências absolutas
  i <- 1:nrow(ni)       # número da linha
  xi <- row.names(ni)   # níveis da variável
  fi <- ni/n            # frequências relativas
  Ni <- cumsum(ni)      # frequências absolutas acumuladas
  Fi <- Ni/n            # frequências relativas acumuladas
#tabela e as frequências relativas com 4 casas decimais (função round())
    tab <- data.frame(i=i, xi =xi, 
                    ni=as.integer(ni), fi=round(as.numeric(fi),4),
                    Ni=as.integer(Ni), Fi=round(as.numeric(Fi),4))
  print(tab)
}

#construir uma função para juntar à função anterior a moda e os gráficos
  
analise.completa <- function(x, nome){
  #tabela de frequências
  print("Tabela de Frequências", quote = FALSE)
  tab1 <- tabela.frequencias(x)
  #moda
  print(paste("Moda:",tab1[which(tab1$ni==max(tab1$ni)),2]), quote = FALSE)
  #gráfico
  barplot(tab1$ni, names.arg=tab1$xi, main=paste(nome), 
          col=2:(nrow(tab1)+1), ylab="Frequências absolutas")
  box(bty = "L")
}

# variáveis qualitativas nominais
analise.completa(obesidade$Genero, "Género")
analise.completa(obesidade$FAVC, "FAVC")
analise.completa(obesidade$Fumar, "Fumar")
analise.completa(obesidade$MTRANS, "MTRANS")

# variáveis qualitativas ordinais
analise.completa(obesidade$FCVC, "FCVC")
analise.completa(obesidade$CH2O, "CH2O")
analise.completa(obesidade$FAF, "FAF")

# variáveis CAEC e CALC 
# os níveis não são números logo ficam ordenados por ordem alfabética
# mas deviam ficar por ordem crescente ou decrescente dos níveis

#factor() -> permite criar variáveis qualitativas e definir a ordem dos níveis

obesidade$CAEC <- factor(obesidade$CAEC, levels = c("N","S","F","A")) 
analise.completa(obesidade$CAEC, "CAEC")

obesidade$CALC <- factor(obesidade$CALC, levels = c("N","S","F","A"))
analise.completa(obesidade$CALC, "CALC")

##################################################

#definir os níveis da variável MTRANS 
#pois no género feminino não há todos os níveis
# (podia ser feito apenas quando tivesse os dados Femininos)

obesidade$MTRANS <- factor(obesidade$MTRANS, 
                           levels = c("A_pe","Automovel","Bicicleta",
                                      "Mota","Transportes_Publicos"))

##################################################

# só os dados do Género Feminino
obesidade.F <- obesidade[obesidade$Genero=="Feminino",]
# só os dados do Género Masculino
obesidade.M <- obesidade[obesidade$Genero=="Masculino",]

# construir uma função para fazer as tabelas de frequências
# calcular a moda e fazer os gráficos por Género

analise.completa.2 <- function(f,m,nome){
  #tabela de frequências -> foi usada a função anterior
  print("Tabela de Frequências - Género Feminino")
  tab2 <- tabela.frequencias(f)
  
  #moda
  print(paste("Moda:",tab2[which(tab2$ni==max(tab2$ni)),2]), quote = FALSE)
  
  #tabela de frequências -> foi usada a função anterior
  print("Tabela de Frequências - Género masculino")
  tab3 <- tabela.frequencias(m)
  
  #moda
  print(paste("Moda:",tab3[which(tab3$ni==max(tab3$ni)),2]), quote = FALSE)
  
  #gráficos de barras com as barras lado a lado
  
  #matrix(): matriz com os dados (podia ser uma data.frame mas o dados são todos numéricos)
  #t(): como os dado foram colocados por coluna foi necessário fazer a transposta da matriz
  #para obter na linha 1 os dados F e na linha 2 os dados M
  ver <- t(matrix(c(tab2$fi,tab3$fi), nrow=nrow(tab2), ncol=2))
  colnames(ver) <- tab2$xi
  
  par(xpd=TRUE)   #para colocar a legenda fora da zona definida para o gráfico
  barplot(ver, beside=T, names.arg=tab2$xi, ylim=c(0,1),
          ylab="frequências relativas",
          main=paste(nome), col=c("red","blue"))
  legend("bottom",
         legend = c("Feminino","Masculino"),
         cex = 0.8,
         fill = c("red","blue"), ncol=2,
         bty = "n",
         inset=c(0,-0.2))
  box(bty = "L")
}

# variáveis qualitativas nominais
analise.completa.2(obesidade.F$FAVC,obesidade.M$FAVC,"FAVC")
analise.completa.2(obesidade.F$Fumar,obesidade.M$Fumar,"Fumar")
analise.completa.2(obesidade.F$MTRANS,obesidade.M$MTRANS, "MTRANS")

# variáveis qualitativas ordinais
analise.completa.2(obesidade.F$FCVC, obesidade.M$FCVC, "FCVC")
analise.completa.2(obesidade.F$CH2O,obesidade.M$CH2O, "CH2O")
analise.completa.2(obesidade.F$FAF, obesidade.M$FAF, "FAF")
analise.completa.2(obesidade.F$CAEC,obesidade.M$CAEC, "CAEC")
analise.completa.2(obesidade.F$CALC,obesidade.M$CALC, "CALC")

#ou

analise.completa.3 <- function(f,m,nome){
  #tabela de frequências -> foi usada a função anterior
  print("Tabela de Frequências - Género Feminino")
  tab2 <- tabela.frequencias(f)
  
  #moda
  print(paste("Moda:",tab2[which(tab2$ni==max(tab2$ni)),2]), quote = FALSE)
  
  #tabela de frequências -> foi usada a função anterior
  print("Tabela de Frequências - Género masculino")
  tab3 <- tabela.frequencias(m)
  
  #moda
  print(paste("Moda:",tab3[which(tab3$ni==max(tab3$ni)),2]), quote = FALSE)
  
  #gráficos de barras com as barras empilhados
  
  #matrix(): matriz com os dados (podia ser uma data.frame mas o dados são todos numéricos)
  #t(): como os dado foram colocados por coluna foi necessário fazer a transposta da matriz
  #para obter na linha 1 os dados F e na linha 2 os dados M
  ver <- t(matrix(c(tab2$fi,tab3$fi), nrow=nrow(tab2), ncol=2))
  colnames(ver) <- tab2$xi

  par(xpd=TRUE)
  barplot(ver, beside=F, names.arg=tab2$xi, ylim=c(0,2),
          ylab="frequências relativas (soma)",
          main=paste(nome), col=c("red","blue"))
  legend("bottom",
         legend = c("Feminino","Masculino"),
         cex = 0.8,
         fill = c("red","blue"), ncol=2,
         bty = "n",
         inset=c(0,-0.2))
  box(bty = "L")
}

# variáveis qualitativas nominais
analise.completa.3(obesidade.F$FAVC,obesidade.M$FAVC,"FAVC")
analise.completa.3(obesidade.F$Fumar,obesidade.M$Fumar,"Fumar")
analise.completa.3(obesidade.F$MTRANS,obesidade.M$MTRANS, "MTRANS")

# variáveis qualitativas ordinais
analise.completa.3(obesidade.F$FCVC, obesidade.M$FCVC, "FCVC")
analise.completa.3(obesidade.F$CH2O,obesidade.M$CH2O, "CH2O")
analise.completa.3(obesidade.F$FAF, obesidade.M$FAF, "FAF")
analise.completa.3(obesidade.F$CAEC,obesidade.M$CAEC, "CAEC")
analise.completa.3(obesidade.F$CALC,obesidade.M$CALC, "CALC")

