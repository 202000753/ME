## PREPARING THE WORKSPACE ##

## OBS: CHANGE PATH##
setwd("C:/Users/nunor/Desktop/ME/Labs/projeto1")
getwd()


## READING DATASET ##
library(readxl)
dfMain<-read_excel("Korea Income and Welfare.xlsx") 
dfJobCode<-read_excel("job_code_translated.xlsx") 

## GENERAL SAMPLE INFO ##

##dfMain general data info##
dim(dfMain)## 92857    14
str(dfMain)
summary(dfMain)## NA's   :33643 +   NA's   :33642  + NA's   :60710  
head(dfMain)
tail(dfMain)

##dfJobCode general data info##
dim(dfJobCode)
str(dfJobCode)
summary(dfJobCode)
head(dfJobCode)
tail(dfJobCode)
unique(dfJobCode$occupation)

##ANALYSIS OF THE VARIABLES BEFORE CLEANING THE SAMPLE DATA##

##VARIABLE id
###VARIABLE CLASSIFICATION
range(dfMain$id)
duplicated(dfMain$id)## THE ID SHOULD BE UNIQUE, THERE ARE DUPLICATED ID FOR DIFFERENT VALUES, WILL DISCART THIS VARIABLE AT THIS TIME 

##VARIABLE year  
###VARIABLE CLASSIFICATION->DISCRETE QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA DISCRETA)
range(dfMain$year)
unique(dfMain$year)

##VARIABLE wave     
###VARIABLE CLASSIFICATION->DISCRETE QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA DISCRETA)
#### DERIVED FROM THE VARIABLE YEAR WHERE 1=2005 AND ONWARDS
range(dfMain$wave)
unique(dfMain$wave)

##VARIABLE region   
###VARIABLE CLASSIFICATION->NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
###*ALTHOUGH IT IS PRESENTED AS A DISCRETE VARIABLE, IT IS ONLY A CODING FOR A NOMINAL QUALITATIVE VARIABLE
#### 1) Seoul 2) Kyeong-gi 3) Kyoung-nam 4) Kyoung-buk 5) Chung-nam 6) Gang-won &. Chung-buk 7) Jeolla & Jeju
range(dfMain$region)
unique(dfMain$region)

##VARIABLE income   
###VARIABLE CLASSIFICATION -> CONTINUOUS QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA CONTINUA)
range(dfMain$income)
unique(dfMain$income)

##VARIABLE family_member   
###VARIABLE CLASSIFICATION->DISCRETE QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA DISCRETA)
range(dfMain$family_member)
unique(dfMain$family_member)

##VARIABLE gender        
###VARIABLE CLASSIFICATION->NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
###*ALTHOUGH IT IS PRESENTED AS A DISCRETE VARIABLE, IT IS ONLY A CODING FOR A NOMINAL QUALITATIVE VARIABLE
#### 1) male 2) female
range(dfMain$gender)
unique(dfMain$gender)

##VARIABLE year_born       
###VARIABLE CLASSIFICATION->DISCRETE QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA DISCRETA)
range(dfMain$year_born)
unique(dfMain$year_born)

##VARIABLE education_level   
###VARIABLE CLASSIFICATION->ORDINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA ORDINAL)
###*ALTHOUGH IT IS PRESENTED AS A DISCRETE VARIABLE, IT IS ONLY A CODING FOR A ORDINAL QUALITATIVE VARIABLE
####  1) no education(under 7 yrs-old) 2) no education(7 & over 7 yrs-old) 3) elementary 4) middle school 5) high school 6) college 7) university degree 8) MA 9) doctoral degree
range(dfMain$education_level)
unique(dfMain$education_level)

##VARIABLE marriage        
###VARIABLE CLASSIFICATION->NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
###*ALTHOUGH IT IS PRESENTED AS A DISCRETE VARIABLE, IT IS ONLY A CODING FOR A NOMINAL QUALITATIVE VARIABLE
#### 1) not applicable (under 18) 2) married 3) separated by death 4) separated 5) not married yet 6) others
range(dfMain$marriage)
unique(dfMain$marriage)

##VARIABLE religion         
###VARIABLE CLASSIFICATION->NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
###*ALTHOUGH IT IS PRESENTED AS A DISCRETE VARIABLE, IT IS ONLY A CODING FOR A NOMINAL QUALITATIVE VARIABLE
#### 1) have religion 2) do not have
range(dfMain$religion)
unique(dfMain$religion)

##VARIABLE occupation       
###VARIABLE CLASSIFICATION->NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
###*ALTHOUGH IT IS PRESENTED AS A DISCRETE VARIABLE, IT IS ONLY A CODING FOR A NOMINAL QUALITATIVE VARIABLE *****ver bem
range(dfMain$occupation)
unique(dfMain$occupation)
####OBS: REPLACE THE 'NA' WITH 0, WE HAVE NO KNOWLEDGE OF WHAT THE OCCUPATION OF THE SUBJECT OF THIS OBSERVATION IS

##VARIABLE company_size      
###VARIABLE CLASSIFICATION->DISCRETE QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA DISCRETA)
range(dfMain$company_size)
unique(dfMain$company_size)
####OBS: REPLACE THE 'NA' WITH -1, WE HAVE NO KNOWLEDGE OF WHAT THE COMPANY SIZE OF THE SUBJECT OF THIS OBSERVATION IS
####THERE ARE NO NEGATIVE COMPANY SIZE

##VARIABLE reason_none_worker  
###VARIABLE CLASSIFICATION->*NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
###*ALTHOUGH IT IS PRESENTED AS A DISCRETE VARIABLE, IT IS ONLY A CODING FOR A NOMINAL QUALITATIVE VARIABLE
#### 1) no capable 2) in military service 3) studying in school 4) prepare for school 5) preprare to apply job 6) house worker 7) caring kids at home 8) nursing 9) giving-up economic activities 10) no intention to work 11) others
range(dfMain$reason_none_worker)
unique(dfMain$reason_none_worker)
####OBS: REPLACE THE 'NA' WITH -1, WE HAVE NO KNOWLEDGE OF WHAT THE NONE WORKER REASON OF THE SUBJECT OF THIS OBSERVATION IS
####THERE ARE NO NEGATIVE VALUES IN NONE WORKER REASON DOMAIN



##CLEANING THE SAMPLE DATA##

###VARIABLE occupation(REPLACE THE 'NA' WITH 0, WE HAVE NO KNOWLEDGE OF WHAT THE OCCUPATION OF THE SUBJECT OF THIS OBSERVATION IS)
dfMain$occupation[is.na(dfMain$occupation)]<-0
range(dfMain$occupation)
unique(dfMain$occupation)
dfMain$occupation[dfMain$occupation == 9999]<-999 ##remove outlier
range(dfMain$occupation)

##VARIABLE company_size(REPLACE THE 'NA' WITH 0, WE HAVE NO KNOWLEDGE OF WHAT THE COMPANY SIZE OF THE SUBJECT OF THIS OBSERVATION IS)
dfMain$company_size[is.na(dfMain$company_size)]<-0
range(dfMain$company_size)
unique(dfMain$company_size)

##VARIABLE reason_none_worker(REPLACE THE 'NA' and the '99' WITH 0, WE HAVE NO KNOWLEDGE OF WHAT THE NONE WORKER REASON OF THE SUBJECT OF THIS OBSERVATION IS)  
dfMain$reason_none_worker[is.na(dfMain$reason_none_worker)]<-0
dfMain$reason_none_worker[dfMain$reason_none_worker == 99]<-0
range(dfMain$reason_none_worker)
unique(dfMain$reason_none_worker)

dim(dfMain) 

## CHARACTERIZATION OF THE SELECTED VARIABLES


##VARIABLE year  
###VARIABLE CLASSIFICATION->DISCRETE QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA DISCRETA)

#FREQUENCY TABLE(TABELA DE FREQUENCIAS)
(ni <- table(dfMain$year)) #frequências absolutas

(xi <- rownames(ni)) #níveis da variável

(n <- sum(ni))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas

(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas


#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)
#DISCRETE QUANTITATIVE VARIABLE (VARIAVEL QUANTITATIVA DISCRETA)

colours<-rainbow(14) # Cria 14 cores diferentes para serem utilizadas nas representações gráficas

# Gráfico de Barras

barplot(ni, names.arg=xi, ylim=c(0,8000), 
        ylab="Frequências Absolutas", xlab="Ano do Estudo",
        main="Gráfico de Barras:
        Ano em que o estudo foi conduzido", 
        col=colours)
box(bty = "L")

boxplot(dfMain$year, range=1.5, col=14)

# Gráfico Circular

pie(ni, labels=paste(xi,"-",round(fi*100), "%"), 
    col=colours,
    main="Gráfico Circular: Ano do Estudo")
legend("topleft", legend=c(xi), 
       fill=colours, cex = 0.55)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: média, mediana e moda

#MODA
if( range(table(dfMain$year))[1]==range(table(dfMain$year))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(dfMain$year)))
}

mean(dfMain$year)#MEDIA

median(dfMain$year)#MEDIANA

#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(dfMain$year, c(0.25,0.50,0.75))#QUARTIS

quantile(dfMain$year, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

var(dfMain$year) #VARIÂNCIA

sd(dfMain$year) #DESVIO PADRÃO

(A = max(dfMain$year)-min(dfMain$year))#AMPLITUDE TOTAL

(AIQ = IQR(dfMain$year))#AMPLITUDE INTERQUARTIL

##Assimetria

library(e1071)
skewness(dfMain$year,type=3)
##Assimetria Negativa* dado que b1<0 (-0.03954851)

## Curtose

kurtosis(dfMain$year,type = 3)
## Curva Platicúrtica* ou achatada dado que b2<0 (-1.182807)

##VARIABLE region   
###VARIABLE CLASSIFICATION->NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
#### 1) Seoul 2) Kyeong-gi 3) Kyoung-nam 4) Kyoung-buk 5) Chung-nam 6) Gang-won &. Chung-buk 7) Jeolla & Jeju

#FREQUENCY TABLE(TABELA DE FREQUENCIAS)

(ni <- table(dfMain$region)) #frequências absolutas

(xi <- c("Seoul","Kyeong-gi","Kyoung-nam","Kyoung-buk","Chung-nam","Gang-won & Chung-buk","Jeolla & Jeju")) #níveis da variável

(n <- sum(ni))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas

(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi, # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas

#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)

colours<-rainbow(7) # Cria 7 cores diferentes para serem utilizadas nas representações gráficas

# Gráfico de Barras

barplot(ni, names.arg=xi, ylim=c(0,20000), 
        ylab="Frequências Absolutas", xlab="Regiões",
        main="Gráfico de Barras: Region", 
        col=colours)
box(bty = "L")


# Gráfico Circular

pie(ni, labels=paste(xi,"-", round(fi*100),"%"), 
    col=colours,
    main="Gráfico Circular: Região")
legend("topright", legend=c(xi), 
       fill=colours, cex = 0.55)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: moda

#MODA
if( range(table(dfMain$region))[1]==range(table(dfMain$region))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(dfMain$region)))
}

##VARIABLE income   
###VARIABLE CLASSIFICATION -> CONTINUOUS QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA CONTINUA)

#FREQUENCY TABLE(TABELA DE FREQUENCIAS)

incomeValues <- as.numeric(dfMain$income)
posValues<- incomeValues[which(incomeValues>=0)]

# Regra Sturges
(n <- length(posValues)) # n = dimensão da amostra

(k<-trunc(1+log(n)/log(2)))# k = número de classes = número de linhas da tabela

# amplitude de cada classe = h
(A = range(posValues)[2]-range(posValues)[1])
(h<-A/k)

max(posValues)

#limites das classes
(cortes <- seq(min(posValues), max(posValues), by=h))

#definir as classes -> cut()
# classes fechadas à direita -> ] , ] = ( , ]
(classes <- cut(posValues, breaks=cortes, right=TRUE, 
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

#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)

# Histograma

(histCortes <- cortes) #não usei os cortes das classes para ser mais legível

hist(posValues, breaks=histCortes,
     main="histograma",
     xlab="Income",
     ylab="frequências absolutas",
     col="gold",
     xlim=c(0,500000), # O maior valor é 468209 (sendo um grande outlier) 
     ylim=c(0,25000))


## Caixa de Bigodes
boxplot(posValues, range=1.5)


#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: média, mediana e moda

#CLASSE MODAL
if(range(tab.frq$ni)[1]==range(tab.frq$ni)[2]){
  print("amodal")
} else{
  print(tab.frq[which(tab.frq$ni==max(tab.frq$ni)),1])
}

mean(posValues)#MEDIA

median(posValues)#MEDIANA

#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(posValues, c(0.25,0.50,0.75))#QUARTIS

quantile(posValues, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

var(posValues) #VARIÂNCIA

sd(posValues) #DESVIO PADRÃO

(A = max(posValues)-min(posValues))#AMPLITUDE TOTAL

(AIQ = IQR(posValues))#AMPLITUDE INTERQUARTIL



##VARIABLE family_member   
###VARIABLE CLASSIFICATION->DISCRETE QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA DISCRETA)

#FREQUENCY TABLE(TABELA DE FREQUENCIAS)
(ni <- table(dfMain$family_member)) #frequências absolutas

(xi <- rownames(ni)) #níveis da variável

(n <- sum(ni))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas

(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas


#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)
#DISCRETE QUANTITATIVE VARIABLE (VARIAVEL QUANTITATIVA DISCRETA)

colours<-rainbow(9) # Cria 14 cores diferentes para serem utilizadas nas representações gráficas

# Gráfico de Barras

barplot(ni, names.arg=xi, ylim=c(0,8000), 
        ylab="Frequências Absolutas", xlab="Anos",
        main="Gráfico de Barras: Years", 
        col=colours)
box(bty = "L")

boxplot(dfMain$family_member, range=1.5, col=14)

# Gráfico Circular

pie(ni, labels=paste(fi*100, "%"), 
    col=colours,
    main="Gráfico Circular: Agregado Familiar")
legend("bottomleft", legend=c(xi), 
       fill=colours, cex = 0.75)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: média, mediana e moda

#MODA
if( range(table(dfMain$family_member))[1]==range(table(dfMain$family_member))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(dfMain$family_member)))
}

mean(dfMain$family_member)#MEDIA

median(dfMain$family_member)#MEDIANA

#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(dfMain$family_member, c(0.25,0.50,0.75))#QUARTIS

quantile(dfMain$family_member, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

var(dfMain$family_member) #VARIÂNCIA

sd(dfMain$family_member) #DESVIO PADRÃO

(A = max(dfMain$family_member)-min(dfMain$family_member))#AMPLITUDE TOTAL

(AIQ = IQR(dfMain$family_member))#AMPLITUDE INTERQUARTIL

skewness(dfMain$family_member,type=3)
##Assimetria Positica dado que b1>0

## Curtose

kurtosis(dfMain$family_member,type = 3)
##  curva platic´urtica, achatada dado que b2<0




##VARIABLE gender        
###VARIABLE CLASSIFICATION->NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
#### 1) male 2) female

#FREQUENCY TABLE(TABELA DE FREQUENCIAS)
(ni <- table(dfMain$gender)) #frequências absolutas

(xi <- rownames(ni)) #níveis da variável

(n <- sum(ni))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas

(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas


#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)
#DISCRETE QUANTITATIVE VARIABLE (VARIAVEL QUANTITATIVA DISCRETA)

colours<-rainbow(2) # Cria 2 cores diferentes para serem utilizadas nas representações gráficas

# Gráfico de Barras

barplot(ni, names.arg=(xi), ylim=c(0,70000), 
        ylab="Frequências Absolutas", xlab="Genero",
        main="Gráfico de Barras: Genero", 
        col=colours)
box(bty = "L")

boxplot(dfMain$gender, range=1.5)

# Gráfico Circular

pie(ni, labels=paste(fi*100, "%"), 
    col=colours,
    main="Gráfico Circular: Years")
legend("topright", legend=c(xi), 
       fill=colours, cex = 0.55)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: média, mediana e moda

#MODA
if( range(table(dfMain$year))[1]==range(table(dfMain$gender))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(dfMain$gender)))
}



#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(dfMain$gender, c(0.25,0.50,0.75))#QUARTIS

quantile(dfMain$gender, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

(A = max(dfMain$gender)-min(dfMain$gender))#AMPLITUDE TOTAL

(AIQ = IQR(dfMain$gender))#AMPLITUDE INTERQUARTIL

##Assimetria

library(e1071)
skewness(dfMain$gender,type=3)
##Assimetria Positiva* dado que b1>0

## Curtose

kurtosis(dfMain$gender,type = 3)
## Curva Platicúrtica* ou achatada dado que b2<0



##VARIABLE year_born       
###VARIABLE CLASSIFICATION->DISCRETE QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA DISCRETA)

#FREQUENCY TABLE(TABELA DE FREQUENCIAS)
(ni <- table(dfMain$year_born)) #frequências absolutas

(xi <- rownames(ni)) #níveis da variável

(n <- sum(ni))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas

(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas


#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)
#DISCRETE QUANTITATIVE VARIABLE (VARIAVEL QUANTITATIVA DISCRETA)

colours<-rainbow(90) # Cria 14 cores diferentes para serem utilizadas nas representações gráficas

# Gráfico de Barras

barplot(ni, names.arg=xi, ylim=c(0,3000), 
        ylab="Frequências Absolutas", xlab="Ano de Nascimento",
        main="Gráfico de Barras: Years", 
        col=colours)
box(bty = "L")

boxplot(dfMain$year_born, range=1.5, col=90)

# Gráfico Circular->ilegivel deve ser tratado como continua classes, poderar se deve ser retirado ou não

pie(ni, labels=paste(ni), 
    col=colours,
    main="Gráfico Circular: Ano de Nascimento")
legend("topright", legend=c(xi), 
       fill=colours, cex = 0.85)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: média, mediana e moda

#MODA
if( range(table(dfMain$year_born))[1]==range(table(dfMain$year_born))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(dfMain$year_born)))
}

mean(dfMain$year_born)#MEDIA

median(dfMain$year_born)#MEDIANA

#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(dfMain$year_born, c(0.25,0.50,0.75))#QUARTIS

quantile(dfMain$year_born, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

var(dfMain$year_born) #VARIÂNCIA

sd(dfMain$year_born) #DESVIO PADRÃO

(A = max(dfMain$year_born)-min(dfMain$year_born))#AMPLITUDE TOTAL

(AIQ = IQR(dfMain$year_born))#AMPLITUDE INTERQUARTIL

##Assimetria

library(e1071)
skewness(dfMain$year_born,type=3)
##Assimetria positiva* dado que b1>0

## Curtose

kurtosis(dfMain$year_born,type = 3)
## Curva Platicúrtica* ou achatada dado que b2<0





##VARIABLE education_level   
###VARIABLE CLASSIFICATION->ORDINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA ORDINAL)
####  1) no education(under 7 yrs-old) 2) no education(7 & over 7 yrs-old) 3) elementary 4) middle school 5) high school 6) college 7) university degree 8) MA 9) doctoral degree


#FREQUENCY TABLE(TABELA DE FREQUENCIAS)
(ni <- table(dfMain$education_level)) #frequências absolutas

(xi <- rownames(ni)) #níveis da variável

(n <- sum(ni))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas

(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas


#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)
#DISCRETE QUANTITATIVE VARIABLE (VARIAVEL QUANTITATIVA DISCRETA)

colours<-rainbow(8) # Cria 8 cores diferentes para serem utilizadas nas representações gráficas

# Gráfico de Barras

barplot(ni, names.arg=(xi), ylim=c(0,30000), 
        ylab="Frequências Absolutas", xlab="Nivel de Educação",
        main="Gráfico de Barras: Nivel de Educação", 
        col=colours)
box(bty = "L")

boxplot(dfMain$education_level, range=1.5)

# Gráfico Circular

pie(ni, labels=paste(fi*100, "%"), 
    col=colours,
    main="Gráfico Circular: Educação")
legend("topright", legend=c(xi), 
       fill=colours, cex = 0.75)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: média, mediana e moda

#MODA
if( range(table(dfMain$education_level))[1]==range(table(dfMain$education_level))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(dfMain$education_level)))
}



#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(dfMain$education_level, c(0.25,0.50,0.75))#QUARTIS

quantile(dfMain$education_level, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

(A = max(dfMain$education_level)-min(dfMain$education_level))#AMPLITUDE TOTAL

(AIQ = IQR(dfMain$education_level))#AMPLITUDE INTERQUARTIL

##Assimetria

library(e1071)
skewness(dfMain$education_level,type=3)
##Assimetria Positiva* dado que b1>0

## Curtose

kurtosis(dfMain$education_level,type = 3)
## Curva Platicúrtica* ou achatada dado que b2<0





##VARIABLE occupation       
###VARIABLE CLASSIFICATION->NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
## prepara a associação das variaveis 

library(dplyr)

dfJobCode <- dfJobCode %>%
  rename(occupation = job_code) ## rename for join
# see new data
summary(dfJobCode)

##leftJoin

newOccupation_df <-left_join(dfMain, dfJobCode, by='occupation')
summary(newOccupation_df)
dim(newOccupation_df)
unique(newOccupation_df$occupation)

##REMOVE VALUE 0 , SO IT WONT LEAD TO WRONG CONCLUSIONS
df_newOccupation <-newOccupation_df$occupation[which(newOccupation_df$occupation!=0)]

summary(df_newOccupation)
unique(df_newOccupation)

#FREQUENCY TABLE(TABELA DE FREQUENCIAS)


(ni <- table(df_newOccupation)) #frequências absolutas

(xi <- rownames(ni)) #níveis da variável

(n <- sum(ni))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas

(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame

# usar as funções: as.integer() e as.numeric()
# para os elementos deixarem de ser do tipo "table" e passarem a ser números

(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas


#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)
#DISCRETE QUANTITATIVE VARIABLE (VARIAVEL QUANTITATIVA DISCRETA)

colours<-rainbow(250) # Cria 250 cores diferentes para serem utilizadas nas representações gráficas

# Gráfico de Barras

barplot(ni, names.arg=(xi), ylim=c(0,12000), 
        ylab="Frequências Absolutas", xlab="Codigo da Ocupação Profissional",
        main="Gráfico de Barras: Ocupação Profissional", 
        col=colours)
box(bty = "L")


## boxplot on a data frame:
df. <- as.data.frame(df_newOccupation)
par(las = 1) # all axis labels horizontal
boxplot(df., main = "Codigo da Ocupação Profissional", horizontal = TRUE)

# Gráfico Circular melhorar

pie(ni, labels=paste(), 
    col=colours,
    main="Gráfico Circular: Ocupação Profissional")
legend("topright", legend=c(xi), 
       fill=colours, cex = 0.75)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)

# medidas de localização central: média, mediana e moda

#MODA
if( range(table(df_newOccupation))[1]==range(table(df_newOccupation))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(df_newOccupation)))
}



#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(df_newOccupation, c(0.25,0.50,0.75))#QUARTIS

quantile(df_newOccupation, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

(A = max(df_newOccupation)-min(df_newOccupation))#AMPLITUDE TOTAL

(AIQ = IQR(df_newOccupation))#AMPLITUDE INTERQUARTIL

##Assimetria

library(e1071)
skewness(df_newOccupation,type=3)
##Assimetria Negativa* dado que b1<0

## Curtose

kurtosis(df_newOccupation,type = 3)
## Curva Platicúrtica* ou achatada dado que b2<0






##VARIABLE company_size      
###VARIABLE CLASSIFICATION->DISCRETE QUANTITATIVE VARIABLE(VARIAVEL QUANTITATIVA DISCRETA)

##REMOVE VALUE 0 , SO IT WONT LEAD TO WRONG CONCLUSIONS
dfCompany_size<-dfMain$company_size[which(dfMain$company_size!=0)]
## SEE NEW DATA
summary(dfCompany_size)
unique(dfCompany_size)



#FREQUENCY TABLE(TABELA DE FREQUENCIAS)
(ni <- table(dfCompany_size))#frequências absolutas
(xi <- rownames(ni))#níveis da variável

(n <- length(dfCompany_size))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas
(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame
(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas


#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)
#DISCRETE QUANTITATIVE VARIABLE (VARIAVEL QUANTITATIVA DISCRETA)
# Gráfico de Barras
barplot(ni, names.arg=xi, ylim=c(0,7000), 
        ylab="FREQUENCIAS ABSOLUTAS", xlab="NR DE EMPREGADOS/TAMANHO DA EMPRESA",
        main="Gráfico de Barras", 
        col=c("blue"))
box(bty = "L")

# Gráfico Circular
pie(ni, labels=paste(fi*100, "%"), 
    col=c("red", "yellow", "blue", "green"),
    main="Gráfico Circular: TAMANHO DA EMPRESA")
legend("topleft", legend=c(xi), 
       fill=c("red", "yellow", "blue", "green"), cex = 0.75)

## Caixa de Bigodes
boxplot(dfCompany_size, range=1.5)



#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)
# medidas de localização central: média, mediana e moda
#MODA
tab.freq[which(tab.freq$ni==max(tab.freq$ni)),2]

mean(dfCompany_size)#MEDIA

median(dfCompany_size)#MEDIANA

#medidas de localizacao não central: Quantis (Quartis e Decis)

quantile(dfCompany_size, c(0.25,0.50,0.75))#QUARTIS

quantile(dfCompany_size, probs = seq(.1, .9, by = .1))#DECIS

# medidas de dispersão: variância, desvio padrão, amplitude total, amplitude interquartil 

var(dfCompany_size) #VARIÂNCIA

sd(dfCompany_size) #DESVIO PADRÃO

(A = max(dfCompany_size)-min(dfCompany_size))#AMPLITUDE TOTAL

(AIQ = IQR(dfCompany_size))#AMPLITUDE INTERQUARTIL

##ASSIMETRIA

skewness(dfCompany_size,type=3)
##Assimetria positiva dado que b1>0

## Curtose

kurtosis(dfCompany_size,type = 3)
## curva leptocúrtica, alongada dado que b2>0


##VARIABLE reason_none_worker  
###VARIABLE CLASSIFICATION->*NOMINAL QUALITATIVE VARIABLE(VARIAVEL QUALITATIVA NOMINAL)
#### 1) no capable 2) in military service 3) studying in school 4) prepare for school 5) preprare to apply job 6) house worker 7) caring kids at home 8) nursing 9) giving-up economic activities 10) no intention to work 11) others

##REMOVE VALUE 0 , SO IT WONT LEAD TO WRONG CONCLUSIONS

dfReason_none_worker<-dfMain$reason_none_worker[which(dfMain$reason_none_worker!=0)]
dfR_none_worker<-dfMain$reason_none_worker[which(dfMain$reason_none_worker<=11)]


## SEE NEW DATA
summary(dfR_none_worker)
unique(dfR_none_worker)


#FREQUENCY TABLE(TABELA DE FREQUENCIAS)
(ni <- table(dfR_none_worker))#frequências absolutas
(xi <- rownames(ni))#níveis da variável

(n <- length(dfR_none_worker))#n = dimensão da amostra

(fi <- ni/n)#frequências relativas

(Ni <- cumsum(ni))# frequências absolutas acumuladas
(Fi <- cumsum(fi))#frequências relativas acumuladas

# tabela de frequências como uma data.frame
(tab.freq <- data.frame(i=1:nrow(ni),       # número da linha
                        xi =xi,             # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi),  # frequências relativas
                        Ni=as.integer(Ni),  # frequências absolutas acumuladas
                        Fi=as.numeric(Fi))) # frequências relativas acumuladas

#GRAPHIC REPRESENTATION(REPRESENTAÇÃO GRÁFICA DOS DADOS)
# Gráfico de Barras
barplot(ni, names.arg=xi, ylim=c(0,20000), 
        ylab="frequências absolutas", xlab="reason_none_worker",
        main="Gráfico de Barras", 
        col=c("blue"))
box(bty = "L")

# Gráfico Circular
pie(ni, labels=paste(fi*100, "%"), 
    col=c("red", "yellow", "blue", "green"),
    main="Gráfico Circular: Reason")
legend("bottomright", legend=c(xi), 
       fill=c("red", "yellow", "blue", "green"), cex = 0.65)

## Caixa de Bigodes
boxplot(dfR_none_worker, range=1.5)

#NUMERICAL LOCATION AND DISPERSION INDICATORS(INDICADORES NUMÉRICOS DE LOCALIZAÇÃO E DISPERSÃO)
#Mediana
median(dfReason_none_worker)

#Moda
tab.freq[which(tab.freq$ni==max(tab.freq$ni)),2]

##ASSIMETRIA

skewness(dfR_none_worker,type=3)
##Assimetria Negativa dado que b1<0

## Curtose

kurtosis(dfR_none_worker,type = 3)
## curva leptocúrtica, alongada dado que b2>0
