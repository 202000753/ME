##################################################
##################################################
#                  Exerc�cio 1                   #
##################################################
##################################################

# vari�vel estat�stica: profiss�o dos participantes no semin�rio
# n�veis: engenheiro, professor, analista de dados, aluno
# classifica��o da vari�vel: qualitativa nominal

##################################################

# c() -> vetor
# data.frame() -> � uma matriz que permite ter colunas de tipos diferentes


(amostra <- c(rep("engenheiro",32), 
              rep("professor",20), 
              rep("analista de dados",16), 
              rep("aluno",12)))

##################################################
# tabela de frequ�ncias

# table() -> obter as frequ�ncias absolutas

# length() -> comprimento de um vetor
# dim() -> dimens�o de uma data.frame
# nrow() -> n�mero de linhas de uma data.frame
# ncol() -> n�mero de colunas de uma data.frame


#frequ�ncias absolutas
(ni <- table(amostra))

# n�veis da vari�vel
(xi <- rownames(ni))

# n = dimens�o da amostra
(n <- length(amostra))
#ou
(n <- sum(ni))

#frequ�ncias relativas
(fi <- ni/n)
#ou
(fi <- prop.table(ni))

# frequ�ncias absolutas acumuladas
(Ni <- cumsum(ni))

#frequ�ncias relativas acumuladas
(Fi <- Ni/n)
#ou
(Fi <- cumsum(fi))


# tabela de frequ�ncias como uma data.frame

# usar as fun��es: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser n�meros

(tab.freq <- data.frame(i=1:nrow(ni),       # n�mero da linha
                        xi =xi,             # n�veis da vari�vel
                        ni=as.integer(ni),  # frequ�ncias absolutas
                        fi=as.numeric(fi),  # frequ�ncias relativas
                        Ni=as.integer(Ni),  # frequ�ncias absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequ�ncias relativas acumuladas


# escrever a tabela num ficheiro txt
write.table(tab.freq, "exer1.txt", quote=FALSE, row.names=F)

# outra possibilidade para fazer a tabela de frequ�ncias: 
# library -> DescTools

library(DescTools)
(tab.freq2 <- Freq(amostra))

  
##################################################

# dados qualitativos nominais:
# gr�ficos de barras -> barplot()
# gr�ficos circulares -> pie()

# abrir outra janela para gr�ficos: x11() ou dev.new()
# fechar a janela de gr�ficos: dev.off()

# gr�ficos de barras -> barplot()
barplot(ni, names.arg=xi, ylim=c(0,40), 
        ylab="frequ�ncias absolutas", xlab="profiss�es",
        main="Gr�fico de Barras", 
        col=c("red", "yellow", "blue", "green"))
box(bty = "L")


# gr�ficos circulares -> pie()
pie(ni, labels=paste(fi*100, "%"), 
    col=c("red", "yellow", "blue", "green"), 
    main="Gr�fico circular")
legend("topright", legend=c(xi), 
       fill=c("red", "yellow", "blue", "green"), cex = 0.7)

# Gr�fico Circular 3D
library(plotrix)
pie3D(ni, col=c("red", "yellow", "green", "blue"),labels=paste(fi*100, "%"), main="Gr�fico Circular 3D",explode=0.1) 
legend("topright", legend=c(xi), fill=c("red", "yellow", "blue", "green"), ncol=2, cex=0.7, bty="n")

# gr�ficos lado a lado
par(mfrow=c(1,2)) 
barplot(ni, names.arg=xi, ylim=c(0,40), 
        ylab="frequ�ncias absolutas", xlab="profiss�es",
        main="Gr�fico de Barras", 
        col=c("red", "yellow", "blue", "green"))
box(bty = "L")
pie(ni, labels=paste(fi*100, "%"), 
    col=c("red", "yellow", "blue", "green"), 
    main="Gr�fico circular")
legend("bottomright", legend=c(xi), 
       fill=c("red", "yellow", "blue", "green"), cex = 0.7)

##################################################

# todo o tipo de gr�ficos e mais bonitos: library -> ggplot2

##################################################
# medidas: moda -> dados qualitativos

# moda 
tab.freq[which(tab.freq$ni==max(tab.freq$ni)),2]

#ou
DescTools::Mode(amostra)


##################################################
##################################################
#                  Exerc�cio 2                   #
##################################################
##################################################

# ler ficheiros EXCEL -> library -> readxl

# setwd() -> para indicar em que diretoria est� o ficheiro ou a trabalhar

library(readxl)
obesidade = read_excel("obesidade.xlsx", sheet=1, col_names=TRUE)

View(obesidade) # ver a tabela

#ou
#no RStudio: File -> Import Dataset

#ver o tipo de dados
str(obesidade)

# ver as primeira linhas
head(obesidade)
# ver as �ltimas linhas
tail(obesidade)

##################################################
#construir uma fun��o para fazer as tabelas de frequ�ncias

tabela.frequencias <- function(x){
  n <- length(x)        # dimens�o da amostra
  ni <- table(x)        # frequ�ncias absolutas
  i <- 1:nrow(ni)       # n�mero da linha
  xi <- row.names(ni)   # n�veis da vari�vel
  fi <- ni/n            # frequ�ncias relativas
  Ni <- cumsum(ni)      # frequ�ncias absolutas acumuladas
  Fi <- Ni/n            # frequ�ncias relativas acumuladas
#tabela e as frequ�ncias relativas com 4 casas decimais (fun��o round())
    tab <- data.frame(i=i, xi =xi, 
                    ni=as.integer(ni), fi=round(as.numeric(fi),4),
                    Ni=as.integer(Ni), Fi=round(as.numeric(Fi),4))
  print(tab)
}

#construir uma fun��o para juntar � fun��o anterior a moda e os gr�ficos
  
analise.completa <- function(x, nome){
  #tabela de frequ�ncias
  print("Tabela de Frequ�ncias", quote = FALSE)
  tab1 <- tabela.frequencias(x)
  #moda
  print(paste("Moda:",tab1[which(tab1$ni==max(tab1$ni)),2]), quote = FALSE)
  #gr�fico
  barplot(tab1$ni, names.arg=tab1$xi, main=paste(nome), 
          col=2:(nrow(tab1)+1), ylab="Frequ�ncias absolutas")
  box(bty = "L")
}

# vari�veis qualitativas nominais
analise.completa(obesidade$Genero, "G�nero")
analise.completa(obesidade$FAVC, "FAVC")
analise.completa(obesidade$Fumar, "Fumar")
analise.completa(obesidade$MTRANS, "MTRANS")

# vari�veis qualitativas ordinais
analise.completa(obesidade$FCVC, "FCVC")
analise.completa(obesidade$CH2O, "CH2O")
analise.completa(obesidade$FAF, "FAF")

# vari�veis CAEC e CALC 
# os n�veis n�o s�o n�meros logo ficam ordenados por ordem alfab�tica
# mas deviam ficar por ordem crescente ou decrescente dos n�veis

#factor() -> permite criar vari�veis qualitativas e definir a ordem dos n�veis

obesidade$CAEC <- factor(obesidade$CAEC, levels = c("N","S","F","A")) 
analise.completa(obesidade$CAEC, "CAEC")

obesidade$CALC <- factor(obesidade$CALC, levels = c("N","S","F","A"))
analise.completa(obesidade$CALC, "CALC")

##################################################

#definir os n�veis da vari�vel MTRANS 
#pois no g�nero feminino n�o h� todos os n�veis
# (podia ser feito apenas quando tivesse os dados Femininos)

obesidade$MTRANS <- factor(obesidade$MTRANS, 
                           levels = c("A_pe","Automovel","Bicicleta",
                                      "Mota","Transportes_Publicos"))

##################################################

# s� os dados do G�nero Feminino
obesidade.F <- obesidade[obesidade$Genero=="Feminino",]
# s� os dados do G�nero Masculino
obesidade.M <- obesidade[obesidade$Genero=="Masculino",]

# construir uma fun��o para fazer as tabelas de frequ�ncias
# calcular a moda e fazer os gr�ficos por G�nero

analise.completa.2 <- function(f,m,nome){
  #tabela de frequ�ncias -> foi usada a fun��o anterior
  print("Tabela de Frequ�ncias - G�nero Feminino")
  tab2 <- tabela.frequencias(f)
  
  #moda
  print(paste("Moda:",tab2[which(tab2$ni==max(tab2$ni)),2]), quote = FALSE)
  
  #tabela de frequ�ncias -> foi usada a fun��o anterior
  print("Tabela de Frequ�ncias - G�nero masculino")
  tab3 <- tabela.frequencias(m)
  
  #moda
  print(paste("Moda:",tab3[which(tab3$ni==max(tab3$ni)),2]), quote = FALSE)
  
  #gr�ficos de barras com as barras lado a lado
  
  #matrix(): matriz com os dados (podia ser uma data.frame mas o dados s�o todos num�ricos)
  #t(): como os dado foram colocados por coluna foi necess�rio fazer a transposta da matriz
  #para obter na linha 1 os dados F e na linha 2 os dados M
  ver <- t(matrix(c(tab2$fi,tab3$fi), nrow=nrow(tab2), ncol=2))
  colnames(ver) <- tab2$xi
  
  par(xpd=TRUE)   #para colocar a legenda fora da zona definida para o gr�fico
  barplot(ver, beside=T, names.arg=tab2$xi, ylim=c(0,1),
          ylab="frequ�ncias relativas",
          main=paste(nome), col=c("red","blue"))
  legend("bottom",
         legend = c("Feminino","Masculino"),
         cex = 0.8,
         fill = c("red","blue"), ncol=2,
         bty = "n",
         inset=c(0,-0.2))
  box(bty = "L")
}

# vari�veis qualitativas nominais
analise.completa.2(obesidade.F$FAVC,obesidade.M$FAVC,"FAVC")
analise.completa.2(obesidade.F$Fumar,obesidade.M$Fumar,"Fumar")
analise.completa.2(obesidade.F$MTRANS,obesidade.M$MTRANS, "MTRANS")

# vari�veis qualitativas ordinais
analise.completa.2(obesidade.F$FCVC, obesidade.M$FCVC, "FCVC")
analise.completa.2(obesidade.F$CH2O,obesidade.M$CH2O, "CH2O")
analise.completa.2(obesidade.F$FAF, obesidade.M$FAF, "FAF")
analise.completa.2(obesidade.F$CAEC,obesidade.M$CAEC, "CAEC")
analise.completa.2(obesidade.F$CALC,obesidade.M$CALC, "CALC")

#ou

analise.completa.3 <- function(f,m,nome){
  #tabela de frequ�ncias -> foi usada a fun��o anterior
  print("Tabela de Frequ�ncias - G�nero Feminino")
  tab2 <- tabela.frequencias(f)
  
  #moda
  print(paste("Moda:",tab2[which(tab2$ni==max(tab2$ni)),2]), quote = FALSE)
  
  #tabela de frequ�ncias -> foi usada a fun��o anterior
  print("Tabela de Frequ�ncias - G�nero masculino")
  tab3 <- tabela.frequencias(m)
  
  #moda
  print(paste("Moda:",tab3[which(tab3$ni==max(tab3$ni)),2]), quote = FALSE)
  
  #gr�ficos de barras com as barras empilhados
  
  #matrix(): matriz com os dados (podia ser uma data.frame mas o dados s�o todos num�ricos)
  #t(): como os dado foram colocados por coluna foi necess�rio fazer a transposta da matriz
  #para obter na linha 1 os dados F e na linha 2 os dados M
  ver <- t(matrix(c(tab2$fi,tab3$fi), nrow=nrow(tab2), ncol=2))
  colnames(ver) <- tab2$xi

  par(xpd=TRUE)
  barplot(ver, beside=F, names.arg=tab2$xi, ylim=c(0,2),
          ylab="frequ�ncias relativas (soma)",
          main=paste(nome), col=c("red","blue"))
  legend("bottom",
         legend = c("Feminino","Masculino"),
         cex = 0.8,
         fill = c("red","blue"), ncol=2,
         bty = "n",
         inset=c(0,-0.2))
  box(bty = "L")
}

# vari�veis qualitativas nominais
analise.completa.3(obesidade.F$FAVC,obesidade.M$FAVC,"FAVC")
analise.completa.3(obesidade.F$Fumar,obesidade.M$Fumar,"Fumar")
analise.completa.3(obesidade.F$MTRANS,obesidade.M$MTRANS, "MTRANS")

# vari�veis qualitativas ordinais
analise.completa.3(obesidade.F$FCVC, obesidade.M$FCVC, "FCVC")
analise.completa.3(obesidade.F$CH2O,obesidade.M$CH2O, "CH2O")
analise.completa.3(obesidade.F$FAF, obesidade.M$FAF, "FAF")
analise.completa.3(obesidade.F$CAEC,obesidade.M$CAEC, "CAEC")
analise.completa.3(obesidade.F$CALC,obesidade.M$CALC, "CALC")

