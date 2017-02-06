## -------------------------------------------------------------------------
## SCRIPT: Sesion 10 Regresion Lineal y Regresion Logistica.R
## CURSO: Master en Data Science
## ASIGNATURA: Regresion lineal y logistica. Modelos lineales generalizados
## PROFESOR: Antonio Pita Lozano
## FECHA: 24/06/2016
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library(ggplot2)
library(effects)
library(plyr)
library(ROCR)

## -------------------------------------------------------------------------
##       PARTE 1: REGRESION LINEAL
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 2. Bloque de carga de datos #####

creditos=read.csv("data/creditos.csv",stringsAsFactors = FALSE)

## -------------------------------------------------------------------------

##### 3. Bloque de revisión basica del dataset #####

str(creditos)
head(creditos)
summary(creditos)

## -------------------------------------------------------------------------

##### 4. Bloque de tratamiento de variables #####

creditos$Gender=as.factor(creditos$Gender)
creditos$Mortgage=as.factor(creditos$Mortgage)
creditos$Married=as.factor(creditos$Married)
creditos$Ethnicity=as.factor(creditos$Ethnicity)

summary(creditos)

## -------------------------------------------------------------------------

##### 5. Bloque de test de diferencia de medias mediante regresion lineal #####

t.test(Income ~ Gender, data = creditos)

# mediante un modelo lineal
modeloT=lm(Income ~ Gender, data = creditos)
summary(modeloT)

## -------------------------------------------------------------------------

##### 6. Bloque de regresion lineal individual #####

modeloInd1=lm(Income ~ Rating, data = creditos)
summary(modeloInd1)

modeloInd2=lm(Income ~ Products, data = creditos)
summary(modeloInd2)

modeloInd3=lm(Income ~ Age, data = creditos)
summary(modeloInd3)

modeloInd4=lm(Income ~ Education, data = creditos)
summary(modeloInd4)

modeloInd5=lm(Income ~ Gender, data = creditos)
summary(modeloInd5)

modeloInd6=lm(Income ~ Mortgage, data = creditos)
summary(modeloInd6)

modeloInd7=lm(Income ~ Married, data = creditos)
summary(modeloInd7)

modeloInd8=lm(Income ~ Ethnicity, data = creditos)
summary(modeloInd8)

modeloInd9=lm(Income ~ Balance, data = creditos)
summary(modeloInd9)

## -------------------------------------------------------------------------

##### 7. Bloque de regresion lineal multiple #####

modeloMul1=lm(Income ~ ., data = creditos)
summary(modeloMul1)

## -------------------------------------------------------------------------

##### 8. Bloque de comparacion de modelos #####

anova(modeloInd1,modeloMul1)

## -------------------------------------------------------------------------

##### 9. Bloque de Ejercicio #####

## ¿Cuales serian las variables que incluiriamos en el modelo?

modeloMul2=lm(Income ~                               , data = creditos)
summary(modeloMul2)

anova(modeloInd1,modeloMul2)
anova(modeloMul2,modeloMul1)

## -------------------------------------------------------------------------

##### 10. Bloque de analisis del modelo #####

modeloFinal=lm(Income ~ Rating+Mortgage+Balance, data = creditos)
summary(modeloFinal)
plot(modeloFinal$residuals)
hist(modeloFinal$residuals)
qqnorm(modeloFinal$residuals); qqline(modeloFinal$residuals,col=2)
confint(modeloFinal,level=0.95)

anova(modeloFinal,modeloMul1)

ggplot(creditos, aes(x = Rating, y = Income)) + geom_point() + facet_grid(~ Mortgage) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(creditos, aes(x = Rating, y = Income)) + geom_point() + facet_grid(~ Ethnicity) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(creditos, aes(x = Balance, y = Income)) + geom_point() + facet_grid(~ Mortgage) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(creditos, aes(x = Balance, y = Income)) + geom_point() + facet_grid(~ Ethnicity) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

## -------------------------------------------------------------------------

##### 11. Bloque de analisis de interacciones #####

modeloInter1=lm(Income ~ Balance+Rating*Mortgage+Balance:Mortgage, data = creditos)
summary(modeloInter1)

modeloInter2=lm(Income ~ Rating*Mortgage+Balance, data = creditos)
summary(modeloInter2)

modeloInter3=lm(Income ~ Rating:Mortgage+Balance, data = creditos)
summary(modeloInter3)

efecto1 <- effect("Rating*Mortgage", modeloInter1, xlevels = 10)
plot(efecto1)

efecto2 <- effect("Balance*Mortgage", modeloInter1, xlevels = 10)
plot(efecto2)

efecto3 <- effect("Rating*Mortgage", modeloInter2, xlevels = 10)
plot(efecto3)

efecto4 <- effect("Rating:Mortgage", modeloInter3, xlevels = 10)
plot(efecto4)

modeloInter5=lm(Income ~ Rating*Mortgage, data = creditos)
summary(modeloInter5)

efecto5 <- effect("Rating*Mortgage", modeloInter5, xlevels = 10)
plot(efecto5)

## -------------------------------------------------------------------------

##### 12. Bloque de analisis de variable Balance #####

modeloBalance=lm(Balance ~ ., data = creditos)
summary(modeloBalance)

## -------------------------------------------------------------------------

##### 13. Bloque de ejercicio #####

## ¿Cuales serian las variables que incluiriamos en el modelo?

modeloBalanceFin=lm(Balance ~                    , data = creditos)
summary(modeloBalanceFin)

## -------------------------------------------------------------------------
##       PARTE 2: MODELOS LINEALES GENERALIZADOS: REGRESION LOGISTICA
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 14. Bloque de carga de datos #####

BANK=read.csv2("data/bank-full.csv")
##### datos extraidos de https://archive.ics.uci.edu/ml/datasets/Bank+Marketing

## -------------------------------------------------------------------------

##### 15. Bloque de revisión basica del dataset #####

str(BANK)
head(BANK)
summary(BANK)

## -------------------------------------------------------------------------

##### 16. Bloque de formateo de variables #####

BANK$day=as.factor(BANK$day)
BANK$campaign=as.factor(BANK$campaign)
BANK$IND_PREVIO=as.factor(as.numeric(BANK$pdays!=-1))

str(BANK)
head(BANK)
summary(BANK)

## -------------------------------------------------------------------------

##### 17. Bloque de modelo de regresión logistica #####

model_logit1=glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=BANK,family=binomial(link="logit"))
summary(model_logit1)
        
model_probit1=glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=BANK,family=binomial(link="probit"))
summary(model_probit1)

# Diferencia entre el logit y el probit
X=seq(from=-4,to=4,by=0.1)
sigmoide=1/(1+exp(-X))
cumulative<-pnorm(X, 0, 1)
plot(sigmoide,type="l",col="red")
lines(cumulative,col="blue")

## -------------------------------------------------------------------------

##### 18. Bloque de evaluación del modelo #####

BANK$prediccion=predict(model_logit1,type="response")
Pred_auxiliar= prediction(BANK$prediccion, BANK$y, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_model_logit1_train = as.numeric(auc.tmp@y.values)
auc_model_logit1_train

CURVA_ROC_model_logit1_train <- performance(Pred_auxiliar,"tpr","fpr")
plot(CURVA_ROC_model_logit1_train,colorize=TRUE)
abline(a=0,b=1)

## Capacidad del Modelo
mean(as.numeric(BANK$y)-1)
aggregate(BANK$prediccion~BANK$y,FUN=mean)

## -------------------------------------------------------------------------

##### 19. Bloque de puesta en valor de un modelo: Fijación del Threshold #####

ALPHA=0.5
Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
Confusion
Accuracy
Precision
Cobertura

ALPHA=0.2
Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
Confusion
Accuracy
Precision
Cobertura

ALPHA=0.8
Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
Confusion
Accuracy
Precision
Cobertura

# Criterio maximizar F1-Score

BANK_KS$Accuracy=(BANK_KS$EXITOS_ACUM+BANK_KS$FRACASOS_TOT-BANK_KS$FRACASOS_ACUM)/BANK_KS$TOTAL
BANK_KS$Precision=BANK_KS$EXITOS_ACUM/BANK_KS$N
BANK_KS$Cobertura=BANK_KS$EXITOS_ACUM/BANK_KS$EXITOS_TOT
BANK_KS$F1Score=2*(BANK_KS$Precision*BANK_KS$Cobertura)/(BANK_KS$Precision+BANK_KS$Cobertura)
plot(BANK_KS$F1Score)
max(BANK_KS$F1Score)
which(BANK_KS$F1Score==max(BANK_KS$F1Score))
BANK_KS[3648,]

## -------------------------------------------------------------------------
##       PARTE 3: MODELOS LINEALES GENERALIZADOS: REGRESION POISSON
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 20. Bloque de carga de datos #####

BICIS=read.csv("data/hour.csv")
##### datos extraidos de https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

## -------------------------------------------------------------------------

##### 21. Bloque de revisión basica del dataset #####

str(BICIS)
head(BICIS)
summary(BICIS)

## -------------------------------------------------------------------------

##### 22. Bloque de modelos de regresión poisson #####

hist(BICIS$cnt)
mean(BICIS$cnt)
sd(BICIS$cnt)

model_poisson=glm(cnt~.-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
summary(model_poisson)

BICIS$prediccion=predict(model_poisson,type="response")
SCE=sum((BICIS$cnt-BICIS$prediccion)^2)
STC=sum((BICIS$cnt-mean(BICIS$cnt))^2)
R2=1-sum((BICIS$cnt-BICIS$prediccion)^2)/sum((BICIS$cnt-mean(BICIS$cnt))^2)

R2

## -------------------------------------------------------------------------

##### 23. Bloque de formateo de variables #####

BICIS$season=as.factor(BICIS$season)
BICIS$yr=as.factor(BICIS$yr)
BICIS$mnth=as.factor(BICIS$mnth)
BICIS$hr=as.factor(BICIS$hr)
BICIS$holiday=as.factor(BICIS$holiday)
BICIS$weekday=as.factor(BICIS$weekday)
BICIS$workingday=as.factor(BICIS$workingday)
BICIS$weathersit=as.factor(BICIS$weathersit)

model_poisson=glm(cnt~.-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
summary(model_poisson)

model_poisson=glm(cnt~.-workingday-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
summary(model_poisson)

BICIS$prediccion=predict(model_poisson,type="response")
SCE=sum((BICIS$cnt-BICIS$prediccion)^2)
STC=sum((BICIS$cnt-mean(BICIS$cnt))^2)
R2=1-sum((BICIS$cnt-BICIS$prediccion)^2)/sum((BICIS$cnt-mean(BICIS$cnt))^2)

R2

## -------------------------------------------------------------------------