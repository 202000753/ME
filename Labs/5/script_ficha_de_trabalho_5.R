##################################################
##################################################
#                  Exercício 1                   #
##################################################
##################################################

# Distribuição Exponencial 
# função densidade de probabilidade: f(x) -> dexp(x,1/theta)
# função de distribuição: F(x)=P(X<=x) -> pexp(x,1/theta)

# X - tempo, em minutos, decorrido entre a passagem de dois veículos no troço da autoestrada
# X~Exp(2)
tx <- 2

##################################################
#alínea 1)

# P(X>2.5)=1-P(X<=2.5)=1-F(2.5)
1-pexp(2.5,1/tx)
#ou
pexp(2.5,1/tx,lower.tail=FALSE)

# OU

# Y - número de veículos que passam no troço da autoestrada em 2.5 minutos
# Y~P(1.25)
(ly=2.5/tx)
# P(Y=0)=f(0)
dpois(0,ly)

##################################################
# alínea 2)

# P(X>=3|X>=2)=P(X>=1)=1-F(1)
1-pexp(1,1/tx)
#ou
pexp(1,1/tx,lower.tail=FALSE)

##################################################
# alínea 3)

# W - número de veículos que passam no troço da autoestrada em 12 minutos
# W~P(6)
(lw=12/tx)
# P(W>5)=1-P(W<=5)=1-F(5)
1-ppois(5,lw)
#ou
ppois(5,lw,lower.tail=FALSE)


##################################################
##################################################
#                  Exercício 2                   #
##################################################
##################################################

# Distribuição Uniforme Contínua 
# função densidade de probabilidade: f(x) -> dunif(x,a,b)
# função de distribuição: F(x)=P(X<=x) -> punif(x,a,b)

# X~U(0,6)
a <- 0
b <- 6

##################################################
# alínea 1)

(k1 = a)
(k2 = b)
(k3 = b-a)

##################################################
# alínea 2)

# P(1.2<=X<5.1|X>=3.4)=(F(5.1)-F(3.4))/(1-F(3.4))
(punif(5.1,a,b)-punif(3.4,a,b))/(1-punif(3.4,a,b))

##################################################
#alínea 3)

# valor esperado de X = E[X]
(EX <- (a+b)/2)
# variância de X = V[X]
(VX <- ((b-a)^2)/12)

# valor esperado de Y = E[Y]=-2E[X]-5
-2*EX-5

# variância de Y = V[Y]=(-2)^2V[X]
(-2)^2*VX

##################################################
##################################################
#                  Exercício 3                   #
##################################################
##################################################

# Distribuição Normal = X~N(media,desvio.padrao)
# função densidade de probabilidade: f(x) -> dnorm(x,media,desvio.padrao)
# função de distribuição: F(x)=P(X<=x) -> pnorm(x,media,desvio.padrao)

# Normal Padrão = Z~N(0,1)
# função densidade de probabilidade: f(x) -> dnorm(x)
# função de distribuição: F(x)=P(X<=x) -> pnorm(x)


# X~N(100,10)
md <- 100
dp <- 10

# Z = (X-media)/desvio.padrao
# Z~N(0,1)

##################################################
# alínea 1)

##################
# usar X~N(100,10) 

#P(60<X<80) = F(80)-F(60)
pnorm(80,md,dp)-pnorm(60,md,dp)


###############
# usar Z~N(0,1) - obrigatório com as tabelas em papel

#P(60<X<80) = P((60-100)/10<Z<(80-100)/10)=F((80-100)/10)-F((60-100)/10)
pnorm((80-md)/dp)-pnorm((60-md)/dp)


##################################################
# alínea 2)

##################
# usar X~N(100,10) 

#P(X>90)=1-P(X<=90)
1-pnorm(90,md,dp)
#ou
pnorm(90,md,dp,lower.tail=FALSE)


##################
# usar Z~N(0,1) - obrigatório com as tabelas em papel

#P(X>90)=P(Z>(90-100)/10)=1-P(Z<=(90-100)/10)
1-pnorm((90-md)/dp)
#ou
pnorm((90-md)/dp,lower.tail=FALSE)


##################################################
# alínea 3)

# quantil de probabilidade (inversa da função de distribuição): qnorm(probabilidade,media,desvio.padrao)


# determinar k tal que P(X>k)=0.025

##################
# usar X~N(100,10) 

# P(X>k)=0.025
qnorm(0.025,md,dp,lower.tail=FALSE)

# ou

# P(X>k)=0.025 <=> 1-P(X<=k)=0.025 <=> 1-F(k)=0.025 <=> F(k)=0.975
qnorm(1-0.025,md,dp)


##################
# usar Z~N(0,1) - obrigatório com as tabelas em papel

# P(X>k)=0.025 <=> P(Z>(k-100)/10)=0.025
# (k-100)/10 é
qnorm(0.025,lower.tail=FALSE)
# k é
(qnorm(0.025,lower.tail=FALSE)*dp)+md

# ou

# P(X>k)=0.025 <=> P(Z>(k-100)/10)=0.025 <=> 1-P(Z<=(k-100)/10)=0.025 <=> 1-F((k-100)/10)=0.025 <=> F((k-100)/10)=0.975
# (k-100)/10 é
qnorm(1-0.025)
# k é
(qnorm(1-0.025)*dp)+md
