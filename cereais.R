library(hnp) # pacote para envelope simulado
library(lmtest) # teste reset
library(car) # para teste de multicolinearidade (fatores de inflacao de variancia)
library(tseries) # teste de Jarque-Bera

dados1<- read.csv("cereal.csv",h=T) # n= 77 cereais
#Vamos utilizar somente:
#calorias, proteinas, gordura, sodio, fibra, açúcares, vitaminas, peso.
#Retirando as outras covariaveis:
dados<-dados1[,c(-1,-2,-3,-9,-11,-13,-15)]
head(dados)
attach(dados)

summary(dados)
cor(dados)
plot(dados)

fit<- lm(rating ~ calories + protein + fat + sodium + fiber + sugars + vitamins 
         + weight, data = dados)
summary(fit)

#Teste de AIC:
step(fit)
fit<-lm(formula = rating ~ protein + fat + sodium + fiber + sugars + 
        vitamins, data = dados)
summary(fit)
#Manteve-se Proteina, Gordura, Sódio, Fibras, Açúcares e Vitaminas.
#Saíram do modelo: Caloria e peso.

#Sumario do Script: coloquei separadores para ficar melhor de mexer no script
#Linha 36: primeiro teste de influencia.
#Linha 132: Retirando a observação 58 e teste AIC
#Linha 160: segundo teste de influencia sem o 58
#Linha 261: Testes das suposiçoes do modelo.

#----

#Testes de influencia:
n<-dim(dados)[1]

### Alavancagem
hatvalues(fit)
h_bar<-fit$rank / n
limite<-2*h_bar
abline(plot(hatvalues(fit),ylab="Alavancagem"), 
       col="red", h=limite,lty=2)
identify(hatvalues(fit),n=2)
#Pontos 4 e 68, mas nada de mais

### DFFIT
dffits(fit)
limite<-2*sqrt(fit$rank / n)
abline(plot(dffits(fit),ylab="DFFITS"), 
       col="red", h=c(-limite,limite),lty=2)
identify(dffits(fit),n=1)
#Um ponto bem distante 58

### DFBETA
dfbetas(fit) # cada beta tem seu DF

dfb1<-dfbetas(fit)[,1]
dfb3<-dfbetas(fit)[,2]
dfb4<-dfbetas(fit)[,3]
dfb5<-dfbetas(fit)[,4]
dfb6<-dfbetas(fit)[,5]
dfb7<-dfbetas(fit)[,6]
dfb8<-dfbetas(fit)[,7]

limite<-2/sqrt(n)
abline(plot(dfb1,ylab="DFBETA 1"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb1,n=2)
# 12 e 58

abline(plot(dfb3,ylab="DFBETA 3"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb3,n=2)
# 58 bem distante e 12 um pouco só

abline(plot(dfb4,ylab="DFBETA 4"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb4,n=1)
# 58

abline(plot(dfb5,ylab="DFBETA 5"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb5,n=1)
# 58

abline(plot(dfb6,ylab="DFBETA 6"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb5,n=1)
# 58

abline(plot(dfb7,ylab="DFBETA 7"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb7,n=1)
# 58

abline(plot(dfb8,ylab="DFBETA 8"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb8,n=2)
# 58 bem distante e 71

## 58 influente em todos os betas

### distancia de Cook

cooks.distance(fit)
limite<-4/(n-fit$rank )
abline(plot(cooks.distance(fit),ylab="Distancia de Cook"), 
       col="red", h=limite,lty=2)
identify(cooks.distance(fit),n=1)
#58 muito influente no modelo

residuo <- rstudent(fit) # residuo studentizado

plot(residuo,type='p',pch="+",main="Residuos",xlab="indices") # plota os residuos do modelo
abline(h=c(-2,0,2),lty=3) # inclui linhas horizontais no grafico
identify(residuo,n=1)
#58 apresentando novamente influencia

hist(residuo) # histograma dos residuos

# envelope simulado baseado nos residuos studentizados
hnp(fit,resid.type="student",halfnormal = F) # envelope simulado 

#Um pouco de ocilação no envolope simulado, contendo pontos fora da banda de 
#confiança. Provavelmente por conta da observação 58.


#-----
#Retirando a observação 58 que foi influente em todos os testes de influencia.

### para comparação:
fit<- lm(rating ~ calories + protein + fat + sodium + fiber + sugars + vitamins 
         + weight, data = dados)
summary(fit)
###

fit<- lm(rating ~ calories + protein + fat + sodium + fiber + sugars + vitamins 
         + weight, data = dados[-58,])
summary(fit)

#Diferenças em praticamente todos os betas, alguns com bastante diferenças,
#outros com poucas diferenças. Em vitaminas ou uma mudança no nivel de 
#significancia indo de 0.01 para 0.001. E uma boa melhora em relação ao R2
#ajustado indo de 0.97 para 0.9908.

# refazendo todos os tentes anteriores:

#Teste de AIC:
step(fit)
fit<-lm(formula = rating ~ protein + fat + sodium + fiber + sugars + 
        vitamins, data = dados[-58, ])
summary(fit)
#Saiu novamente calorias e peso.


#----
#Testes de influencia:
n<-dim(dados[-58,])[1]
#Obs: Lembrando que todas as observaçoes apartir de 58 vao se ajustar, 
#entao nos dados gerais todaa observação será um numero a mais apartir de 58
#ex: 67 é a observação 68 nos dados gerais.

### Alavancagem
hatvalues(fit)
h_bar<-fit$rank / n
limite<-2*h_bar
abline(plot(hatvalues(fit),ylab="Alavancagem"), 
       col="red", h=limite,lty=2)
identify(hatvalues(fit),n=4)
#Pontos 2, 4, 12 e 67, mas nada de mais

### DFFIT
dffits(fit)
limite<-2*sqrt(fit$rank / n)
abline(plot(dffits(fit),ylab="DFFITS"), 
       col="red", h=c(-limite,limite),lty=2)
identify(dffits(fit),n=1)
#4 nada de mais

### DFBETA
dfbetas(fit) # cada beta tem seu DF

dfb1<-dfbetas(fit)[,1]
dfb3<-dfbetas(fit)[,2]
dfb4<-dfbetas(fit)[,3]
dfb5<-dfbetas(fit)[,4]
dfb6<-dfbetas(fit)[,5]
dfb7<-dfbetas(fit)[,6]
dfb8<-dfbetas(fit)[,7]

limite<-2/sqrt(n)
abline(plot(dfb1,ylab="DFBETA 1"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
#identify(dfb1,n=2)
# Nada muito distante

abline(plot(dfb3,ylab="DFBETA 3"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb3,n=1)
# 67

abline(plot(dfb4,ylab="DFBETA 4"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb4,n=3)
# 31 35 67 nada muito longe

abline(plot(dfb5,ylab="DFBETA 5"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb5,n=1)
# 31

abline(plot(dfb6,ylab="DFBETA 6"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb5,n=2)
# 4 bem distante e 67

abline(plot(dfb7,ylab="DFBETA 7"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb6,n=1)
# 31

abline(plot(dfb8,ylab="DFBETA 8"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))
identify(dfb7,n=1)
# 70

## Valores bem variados, mas podemos pensar em testar 2 valores 4 e 67(68 no geral)

### distancia de Cook

cooks.distance(fit)
limite<-4/(n-fit$rank )
abline(plot(cooks.distance(fit),ylab="Distancia de Cook"), 
       col="red", h=limite,lty=2)
identify(cooks.distance(fit),n=4)
# 4 com bastante influencia aqui, 67 70 e 31 mais ou menos.

residuo <- rstudent(fit) # residuo studentizado

plot(residuo,type='p',pch="+",main="Residuos",xlab="indices") # plota os residuos do modelo
abline(h=c(-2,0,2),lty=3) # inclui linhas horizontais no grafico
identify(residuo,n=4)
#Não temos nenhum dos pontos observados antes como possiveis influentes.

hist(residuo) # histograma dos residuos
#Aqui o histograma já ficou bem melhor, tendendo a uma normal.

# envelope simulado baseado nos residuos studentizados
hnp(fit,resid.type="student",halfnormal = F) # envelope simulado 
#Aqui já vemos que o envelope simudado já está bem ajustado. Com todas
#os pontos praticamente dentro da banda de confiança.

#Obs: Refiz os testes para as observaçoes 4 e 68 e n apresentaram 
#influencia/melhora no ajuste do modelo. Portanto só mantera o 58 retirado 
#dos dados.

#-----

#Testar as suposiçoes do modelo:
fit<-lm(formula = rating ~ protein + fat + sodium + fiber + sugars + 
          vitamins, data = dados[-58, ])
summary(fit)

## Testa [S0]
## Teste RESET de especificacao
## H0: O modelo estah corretamente especificado
resettest(fit)
#p-value = 0.06261 > alpha=0.05
#Validamos H0


## Testa [S1]
## Teste t para a media dos erros
## H0: media dos erros eh igual a zero
t.test(residuo,mu=0,alternative="two.sided")
#p-value = 0.9966 > alpha=0.05
#Validamos H0


## Testa [s2]
## Teste de Bressch-Pagan (Koenker) de Heteroscedasticidade
## H0: erros sao homoscedasticos
bptest(fit, studentize = TRUE)
#p-value = 0.962  > alpha=0.05
#Validamos H0


## Testa [S3]
## Teste de Durbin-Watson de autocorrelacao
## H0: : Nao hah autocorrelacao 
dwtest(fit)
#p-value = 0.404 > alpha=0.05
#Validamos H0


## Testa [S4]
## Usa Fatores de Inflacao de Variancia para detectar multicolinearidade
## Regra de bolsa: vif > 10 indica multicolinearidade. vif=1 seria o ideal.
vif(fit)
#protein      fat   sodium    fiber   sugars vitamins 
#1.634900 1.234248 1.137387 1.382335 1.307380 1.156292 
#Tudo proximo de 1, Sem multicolinearidade(ideal).

## Testa [S5]
## Teste Jarque-Bera de Normalidade
## H0: Os erros possuem distribuicao normal
jarque.bera.test(residuo)
#p-value = 0.8215 > alpha=0.05
#Validamos H0

#Modelo está valido em todas as suposiçoes.

