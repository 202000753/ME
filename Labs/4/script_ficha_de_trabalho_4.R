##################################################
##################################################
#                  Exercício 1                   #
##################################################
##################################################


##################################################
#alínea 1)

# X - número de pontos obtidos num único lançamento de um dado
# domínio = D = {1,2,3,4,5,6}

library(MASS)  # para usar a função "fractions" e escrever em fração em vez de decimal

# função de probabilidade
# f(1) = F(1)
(f1 <- fractions(1/6))

# f(2) = F(2)-F(1)
(f2 <- fractions((1/3)-(1/6)))

# f(3) = F(3)-F(2)
(f3 <- fractions((1/2)-(1/3)))

# f(4) = F(4)-F(3)
(f4 <- fractions((2/3)-(1/2)))

# f(5) = F(5)-F(4)
(f5 <- fractions((5/6)-(2/3)))

# f(6) = F(6)-F(5)
(f6 <- fractions(1-(5/6)))


(fx <- data.frame(x=1:6, 
                  fx=c(f1,f2,f3,f4,f5,f6)))

# todos os elementos do domínio têm igual probabilidade de acontecer
# distribuição Uniforme discreta 
# X~U(6) -> o dado não é viciado

##################################################

# Distribuição Uniforme Discreta 
library(extraDistr)
# só está implementada para domínios cujos elementos são inteiros consecutivos 
# função de probabilidade: f(x)=P(X=x) -> ddunif(x,min,max)
# função de distribuição: F(x)=P(X<=x) -> pdunif(x,min,max)

# domínio = D = {1,2,3,4,5,6}
# domínio cujos elementos são inteiros consecutivos

# min = a 
a <- 1
# max = b 
b <- 6

# calcule P(X=5)=f(5)
ddunif(5,a,b)

# calcule P(X<5)=P(X<=4)=F(4)
pdunif(4,a,b)

# calcule P(X>5)=1-P(X<=5)=1-F(5)
1-pdunif(5,a,b)
#ou
pdunif(5,a,b,lower.tail=FALSE)


##################################################
#alínea 2)

# domínio = D = {1,2,3,4,5,6}
# domínio cujos elementos são inteiros consecutivos
# min = a  e   max = b 

# valor esperado de X = E[X]
(EX <- (a+b)/2)
# variância de X = V[X]
(VX <- (((b-a+1)^2)-1)/12)

# valor esperado de Y = E[Y]=(-2/5)E[X]-1/5
(-2/5)*EX-1/5

# variância de Y = V[Y]=(-2/5)^2V[X]
(-2/5)^2*VX

##################################################
##################################################
#                  Exercício 2                   #
##################################################
##################################################

# Distribuição Binomial 
# função de probabilidade: f(x)=P(X=x) -> dbinom(x,n,p)
# função de distribuição: F(x)=P(X<=x) -> pbinom(x,n,p)

# X - número de cartões de memória com defeito numa caixa
# X~B(10,0.1)
nx <- 10
px <- 0.1

##################################################
#alínea 1)
# P(X>1)=1-P(X<=1)=1-F(1)
1-pbinom(1,nx,px)
#ou
pbinom(1,nx,px,lower.tail=FALSE)

##################################################
#alínea 2)
# P(X=0)=f(0)
dbinom(0,nx,px)

##################################################
#alínea 3)
# P(X<=2|X>0) =(f(2)+f(1))/(1-f(0))
(dbinom(2,nx,px)+dbinom(1,nx,px))/(1-dbinom(0,nx,px))

##################################################
#alínea 4)
#P(2<=X<=5)=P(1<X<=5)=F(5)-F(1)
pbinom(5,nx,px)-pbinom(1,nx,px)

##################################################
#alínea 5)
# P(3<X<7)=P(3<X<=6)=F(6)-F(3)
pbinom(6,nx,px)-pbinom(3,nx,px)

##################################################
#alínea 6) 
#a)
# Y - número de caixas devolvidas, num conjunto de 3 caixas
# Y~B(3,py)
# py = P(X>1)=1-P(X<=1)=1-F(1)
ny <- 3
(py <- 1-pbinom(1,nx,px))

# P(Y=1)=f(1)
dbinom(1,ny,py)

#b)
#média
ny*py
# desvio padrão
sqrt(ny*py*(1-py))

##################################################
##################################################
#                  Exercício 3                   #
##################################################
##################################################

# Distribuição de Poisson 
# função de probabilidade: f(x)=P(X=x) -> dpois(x,lambda)
# função de distribuição: F(x)=P(X<=x) -> ppois(x,lambda)

# X - número de leituras enviadas, durante uma hora, por um satélite para um centro de meteorologia
# X~P(5)
lx <- 5

##################################################
#alínea 2)
# P(X=5)=f(5)
dpois(5,lx)

##################################################
#alínea 3)
# Y - número de leituras enviadas, num intervalo de 10 minutos, por um satélite para um centro de meteorologia
# Y~P(5/6)    ly=10*5/60=5/6
# P(Y=0)=f(0)
(ly <- 10*lx/60)
dpois(0,ly)

##################################################
#alínea 4)
# W - número de leituras enviadas, em 90 minutos, por um satélite para um centro de meteorologia
# W~P(15/2)    lw=90*5/60=15/2
# P(W=10)=f(10)
(lw <- 90*lx/60)
dpois(10,lw)

##################################################
#alínea 5)
# T - número de leituras enviadas, em meia hora, por um satélite para um centro de meteorologia
# T~P(5/2)    lt=30*5/60=5/2
# P(T>=2)=1-P(T<2)=1-P(T<=1)=1-F(1) 
(lt <- 30*lx/60)
1-ppois(1,lt)
#ou
ppois(2,lt, lower.tail=FALSE)+dpois(2,lt)

##################################################
#alínea 6)
# P(2<X<6)=P(2<X<=5)=F(5)-F(2)
ppois(5,lx)-ppois(2,lx)

##################################################
#alínea 7)
# V - número de leituras enviadas, num determinado período de tempo, por um satélite para um centro de meteorologia
# V~P(lambda)
# pretende-se determinar lambda tal que
# P(V=0)=0.005 <=> f(0)=0.005

# f(x)=lambda^x exp(-lambda)/x!
# f(0)=lambda^0 exp(-lambda)/0!= exp(-lambda)
# f(0)=0.005 <=> exp(-lambda)=0.005 <=> lambda=-ln(0.005)
-log(0.005)

dpois(0,-log(0.005))  #verificar que f(0)=0.005

# como lambda=tempo*5/60 então o intervalo de tempo é
# -ln(0.005)=tempo*5/60 <=> tempo = -ln(0.005)/ (5/60) minutos
-log(0.005)/(5/60)

