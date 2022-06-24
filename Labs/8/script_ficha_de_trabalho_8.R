#######################################################################################
#######################################################################################
#######################################################################################

#                       Teste de Independência do Qui-Quadrado                        #

#######################################################################################
#######################################################################################
#######################################################################################

# funçãoo -> chisq.test()

# argumentos da função: tabela de contingência

#######################################################################################

options(scipen = 999) # para não escrever os números em notação cientifica

#######################################################################################

#######################################################################
#######################################################################
#                            Exercício 1                              #
#######################################################################
#######################################################################

# H0:os pais praticarem ou não desporto é independente dos filhos praticarem ou não desporto
# contra
# H1:os pais praticarem ou não desporto NÃO é independente dos filhos praticarem ou não desporto

library(readxl)
dados1 <- read_excel("dados - Teste de independencia do Qui-Quadrado.xlsx", 
                     sheet = "desporto")
View(dados1)
dim(dados1)

# construir a tabela de contingência
(tabela.1 <- table(dados1))
dimnames(tabela.1) = list(pai=c("Não","Sim"),
                          filho=c("Não","Sim"))
tabela.1

###################################################

# teste de Independência do Qui-Quadrado
chisq.test(tabela.1, correct = FALSE)

exemplo.1 <- chisq.test(tabela.1, correct = FALSE)
exemplo.1$statistic # Qobs
exemplo.1$parameter # graus de liberdade
exemplo.1$p.value   # valor-p
exemplo.1$observed  # Oi = frequências Observadas
exemplo.1$expected  # Ei = frequências Esperadas

#################

#tabelas que estamos a comparar

exemplo.1$observed
exemplo.1$expected


#####################
# tomar uma decisão #
#####################

#alínea a)
exemplo.1$p.value
# valor-p > alpha -> Não se rejeita H0

#alínea b)
alpha <- 0.05
# a região crítica começa:
qchisq(1-alpha,exemplo.1$parameter)
#RA=[0,3.84[ e RC=[3.84,+infinito[
# valor observado
exemplo.1$statistic
# valor observado pertence à RA -> Não se rejeita H0

#alínea c)
exemplo.1$p.value
# a partir do valor-p
# pois Rejeita-se H0 se alpha >= valor_p

#######################################################################
#######################################################################
#                            Exercício 2                              #
#######################################################################
#######################################################################

# H0:o número de reprovações é independente do número de faltas
# contra
# H1:o número de reprovações NÃO é independente do número de faltas

dados2 <- read_excel("dados - Teste de independencia do Qui-Quadrado.xlsx", 
                     sheet = "insucesso escolar")
View(dados2)
dim(dados2)

#colocar os níveis da variável por ordem
dados2$faltas <- factor(dados2$faltas, 
                           levels = c("nenhuma","algumas","muitas"))

# construir a tabela de contingência
(tabela.2 <- table(dados2))
dimnames(tabela.2) = list(reprovacao=c("nenhuma","uma","duas ou mais"),
                          faltas=c("nenhuma","algumas","muitas"))

tabela.2


###################################################

# teste de Independência do Qui-Quadrado
chisq.test(tabela.2, correct = FALSE)

exemplo.2 <- chisq.test(tabela.2, correct = FALSE)
exemplo.2$statistic # Qobs
exemplo.2$parameter # graus de liberdade
exemplo.2$p.value   # valor-p
exemplo.2$observed  # Oi = frequências Observadas
exemplo.2$expected  # Ei = frequências Esperadas

#################

#tabelas que estamos a comparar

exemplo.2$observed
exemplo.2$expected


#####################
# tomar uma decisão #
#####################

#alínea a)
exemplo.2$p.value
# valor-p < alpha -> Rejeita-se H0

#alínea b)
alpha <- 0.05
# a região crítica começa:
qchisq(1-alpha,exemplo.2$parameter)
#RA=[0,9.49[ e RC=[9.49,+infinito[
# valor observado
exemplo.2$statistic
# valor observado pertence à RC -> Rejeita-se H0

#alínea c)
exemplo.2$p.value
# a partir do valor-p
# pois Rejeita-se H0 se alpha >= valor_p
