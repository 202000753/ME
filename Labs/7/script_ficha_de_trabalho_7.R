#######################################################################################
#######################################################################################
#######################################################################################

#                      Teste de Ajustamento de Kolmogorv-Smirnov                      #

#######################################################################################
#######################################################################################
#######################################################################################

# função -> ks.test()

# argumento da função: amostra e função de distribuição da hipótese nula

#######################################################################################


#######################################################################
#######################################################################
#                            Exercício 1                              #
#######################################################################
#######################################################################

# H0:X segue uma distribuição Exponencial de média 730
# contra
# H1:X NÃO segue uma distribuição Exponencial de média 730

library(readxl)
dados1 <- read_excel("dados - Teste de ajustamento de Kolmogorov-Smirnov.xlsx", 
                     sheet = "tempos de falha")
View(dados1)
dim(dados1)

#dimensão da amostra
(n <- length(dados1$tempos))

# teste de Ajustamento de Kolmogorv-Smirnov
ks.test(dados1$tempos, "pexp", rate=1/730)

exemplo.1 <- ks.test(dados1$tempos, "pexp", rate=1/730)

exemplo.1$statistic # Dobs
exemplo.1$p.value  # valor-p

#################

#tabela que estamos a comparar

dados <- DescTools::Freq(factor(dados1$tempos))

data.frame(FS=dados$cumperc,
           FT=pexp(sort(dados1$tempos),1/730))

#graficamente o que estamos a comparar
# gráfico das frequências relativas acumuladas e da função de distribuião da distribuição de Exponencial de média 730

plot(ecdf(dados1$tempos), col="red", main="", ylab="")
curve(pexp(x, rate=1/730), from=min(dados1$tempos), col="green",lwd=4, add=TRUE)# função distribuição da distribuição exponencial
legend("bottomright", 
       legend=c("observado", "esperado"), 
       lty=c(1,1),lwd=c(4,4),
       col=c("red", "green"))


#####################
# tomar uma decisão #
#####################

#alínea a)
exemplo.1$p.value
# valor-p > alpha -> Não se rejeita H0

#alínea b)
# a região crítica começa:
0.304  # ver na tabela em papel
#RA=[0,0.304[ e RC=[0.304,+infinito[
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

# H0:X segue uma distribuição Normal com média 80 e desvio padrão 6.95
# contra
# H1:X NÃO segue uma distribuição Normal com média 80 e desvio padrão 6.95

dados2 <- read_excel("dados - Teste de ajustamento de Kolmogorov-Smirnov.xlsx", 
                     sheet = "niveis de salinidade")
View(dados2)
dim(dados2)

#dimensão da amostra
(n <- length(dados2$salinidade))

# teste de Ajustamento de Kolmogorv-Smirnov
ks.test(dados2$salinidade, "pnorm", mean=80, sd=6.95)

exemplo.2 <- ks.test(dados2$salinidade, "pnorm", mean=80, sd=6.95)

exemplo.2$statistic # Dobs
exemplo.2$p.value  # valor-p

#################

#tabela que estamos a comparar

dados <- DescTools::Freq(factor(dados2$salinidade))
dados.r <- dados2[!duplicated(dados2),]  # retirar os valores repetidos

data.frame(FS=dados$cumperc,
           FT=pnorm(sort(dados.r$salinidade),80,6.95))

#graficamente o que estamos a comparar
# gráfico das frequências relativas acumuladas e da função de distribuião da distribuição de Exponencial de média 730

plot(ecdf(dados2$salinidade), col="red", main="", ylab="")
curve(pnorm(x, 80,6.95), from=min(dados2$salinidade), col="green",lwd=4, add=TRUE)# função distribuição da distribuição normal
legend("bottomright", 
       legend=c("observado", "esperado"), 
       lty=c(1,1),lwd=c(4,4),
       col=c("red", "green"))


#####################
# tomar uma decisão #
#####################

#alínea a)
exemplo.2$p.value
# valor-p > alpha -> Não se rejeita H0

#alínea b)
# a região crítica começa:
0.221  # ver na tabela em papel
#RA=[0,0.221[ e RC=[0.221,+infinito[
# valor observado:
exemplo.2$statistic
# valor observado pertence à RA -> Não se rejeita H0

#alínea c)
exemplo.2$p.value
# a partir do valor-p
# pois Rejeita-se H0 se alpha >= valor_p
