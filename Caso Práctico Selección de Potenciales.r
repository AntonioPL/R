## -------------------------------------------------------------------------
## SCRIPT: Caso Practico Seleccion de Potenciales.R
## CURSO: Master en Data Science
## ASIGNATURA: Casos de exito de negocio en Data Science
## PROFESOR: Antonio Pita Lozano
## FECHA: 30/09/2016
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------
##       PARTE 1: ANALISIS DESCRIPTIVO DE DATOS
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias y establecimiento de directorio #####

library(ROCR)
library(caTools)
library(dplyr)

setwd("D:/Documentos, Trabajos y Demás/Formación/Kschool/20160624 Clase II Master Data Science/Casos de Exito de Negocio")

## -------------------------------------------------------------------------

##### 2. Bloque de carga de datos #####

Depositos=read.csv2("Campaña Deposito Especial.csv")

## -------------------------------------------------------------------------

##### 3. Bloque de revisión basica del dataset #####

str(Depositos)
head(Depositos)
summary(Depositos)
tail(Depositos)

## -------------------------------------------------------------------------

##### 4. Bloque de formateo de variables #####

Depositos$CAMP_DEPOSITOS=as.factor(Depositos$CAMP_DEPOSITOS)

str(Depositos)
head(Depositos)
summary(Depositos)
tail(Depositos)

## -------------------------------------------------------------------------

##### 5. Bloque de creacion de conjuntos de entrenamiento, validacion y test #####

set.seed(1234) 
SAMPLE = sample.split(Depositos$CAMP_DEPOSITOS, SplitRatio = .60)
DepositosTrain = subset(Depositos, SAMPLE == TRUE)
DepositosValTest = subset(Depositos, SAMPLE == FALSE)
set.seed(1234)
SAMPLE = sample.split(DepositosValTest$CAMP_DEPOSITOS, SplitRatio = .50)
DepositosVal= subset(DepositosValTest, SAMPLE == TRUE)
DepositosTest = subset(DepositosValTest, SAMPLE == FALSE)

dim(Depositos)
dim(DepositosTrain)
dim(DepositosVal)
dim(DepositosTest)

## -------------------------------------------------------------------------

##### 6. Bloque de analisis de conjuntos de entrenamiento, validacion y test #####

table(Depositos$CAMP_DEPOSITOS)
sum(Depositos$CAMP_DEPOSITOS==1)/length(Depositos$CAMP_DEPOSITOS)

table(DepositosTrain$CAMP_DEPOSITOS)
prior=sum(DepositosTrain$CAMP_DEPOSITOS==1)/length(DepositosTrain$CAMP_DEPOSITOS)
prior

table(DepositosVal$CAMP_DEPOSITOS)
sum(DepositosVal$CAMP_DEPOSITOS==1)/length(DepositosVal$CAMP_DEPOSITOS)

table(DepositosTest$CAMP_DEPOSITOS)
sum(DepositosTest$CAMP_DEPOSITOS==1)/length(DepositosTest$CAMP_DEPOSITOS)

## -------------------------------------------------------------------------
##       PARTE 2: SELECCIÓN DE VARIABLES
##                  CAPACIDAD PREDICTIVA DE LAS VARIABLES
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 7. Bloque de funcion de relevancia #####

relevancia=function(Target,VariableCategorica){
  levels=levels(VariableCategorica)
  colors=c()
  for (i in 1:length(levels)){
    TABLA=table(Target,VariableCategorica==levels[i])
    chi=chisq.test(TABLA)
    if (chi$p.value<0.05){
      colors=c(colors,"green")
    }else{
      colors=c(colors,"gray")
    }
  }
  TABLA=table(Target,VariableCategorica)
  plot=barplot(100*TABLA[2,]/(TABLA[1,]+TABLA[2,]),ylim=c(0,100),col=colors,cex.names=0.6)
  text(x=plot, y=5+100*TABLA[2,]/(TABLA[1,]+TABLA[2,]),labels=paste(round(100*TABLA[2,]/(TABLA[1,]+TABLA[2,]),2),"%",sep=""))
  abline(h=100*prior,col="red")
}

## -------------------------------------------------------------------------

##### 8. Bloque de relevancia de las variables #####

dev.off()
png("./100.Grafico Relevancia Edad.png",width = 1024, height = 880)
relevancia(DepositosTrain$CAMP_DEPOSITOS,DepositosTrain$CAT_EDAD)
dev.off()
png("./100.Grafico Relevancia Nivel Estudios.png",width = 1024, height = 880)
relevancia(DepositosTrain$CAMP_DEPOSITOS,DepositosTrain$NIVEL_ESTUDIO)
dev.off()
png("./100.Grafico Relevancia Rango Ingresos.png",width = 1024, height = 880)
relevancia(DepositosTrain$CAMP_DEPOSITOS,DepositosTrain$RANGO_INGRESOS)
dev.off()
png("./100.Grafico Relevancia Estado Civil.png",width = 1024, height = 880)
relevancia(DepositosTrain$CAMP_DEPOSITOS,DepositosTrain$ESTADO_CIVIL)
dev.off()
png("./100.Grafico Relevancia Sexo.png",width = 1024, height = 880)
relevancia(DepositosTrain$CAMP_DEPOSITOS,DepositosTrain$SEXO)
dev.off()

prior

## -------------------------------------------------------------------------

##### 9. Bloque de modelos de regresión logística #####

modelo1=glm(CAMP_DEPOSITOS~RANGO_INGRESOS, data=DepositosTrain[,-1],family=binomial(link="logit"))
summary(modelo1)
modelo2=glm(CAMP_DEPOSITOS~RANGO_INGRESOS+NIVEL_ESTUDIOS, data=DepositosTrain[,-1],family=binomial(link="logit"))
summary(modelo2)
modelo3=glm(CAMP_DEPOSITOS~RANGO_INGRESOS+NIVEL_ESTUDIOS+CAT_EDAD, data=DepositosTrain[,-1],family=binomial(link="logit"))
summary(modelo3)
modelo4=glm(CAMP_DEPOSITOS~RANGO_INGRESOS+NIVEL_ESTUDIOS+CAT_EDAD+ESTADO_CIVIL, data=DepositosTrain[,-1],family=binomial(link="logit"))
summary(modelo4)
modelo5=glm(CAMP_DEPOSITOS~., data=DepositosTrain[,-1],family=binomial(link="logit"))
summary(modelo5)

## -------------------------------------------------------------------------

##### 10. Bloque de comparación y selección de modelos de regresión logística #####

AIC(modelo1)
AIC(modelo2)
AIC(modelo3)
AIC(modelo4)
AIC(modelo5)

BIC(modelo1)
BIC(modelo2)
BIC(modelo3)
BIC(modelo4)
BIC(modelo5)

## -------------------------------------------------------------------------

##### 11. Bloque de evaluación del modelo #####

prediccion=predict(modelo1,type="response")
Pred_auxiliar= prediction(prediccion, DepositosTrain$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo1_train = as.numeric(auc.tmp@y.values)
auc_modelo1_train

prediccion=predict(modelo1, newdata=DepositosVal,type="response")
Pred_auxiliar = prediction(prediccion, DepositosVal$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo1_val = as.numeric(auc.tmp@y.values)
auc_modelo1_val

prediccion=predict(modelo2,type="response")
Pred_auxiliar= prediction(prediccion, DepositosTrain$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo2_train = as.numeric(auc.tmp@y.values)

prediccion=predict(modelo2, newdata=DepositosVal,type="response")
Pred_auxiliar = prediction(prediccion, DepositosVal$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo2_val = as.numeric(auc.tmp@y.values)

prediccion=predict(modelo3,type="response")
Pred_auxiliar= prediction(prediccion, DepositosTrain$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo3_train = as.numeric(auc.tmp@y.values)

prediccion=predict(modelo3, newdata=DepositosVal,type="response")
Pred_auxiliar = prediction(prediccion, DepositosVal$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo3_val = as.numeric(auc.tmp@y.values)

prediccion=predict(modelo4,type="response")
Pred_auxiliar= prediction(prediccion, DepositosTrain$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo4_train = as.numeric(auc.tmp@y.values)

prediccion=predict(modelo4, newdata=DepositosVal,type="response")
Pred_auxiliar = prediction(prediccion, DepositosVal$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo4_val = as.numeric(auc.tmp@y.values)

prediccion=predict(modelo5,type="response")
Pred_auxiliar= prediction(prediccion, DepositosTrain$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo5_train = as.numeric(auc.tmp@y.values)

prediccion=predict(modelo5, newdata=DepositosVal,type="response")
Pred_auxiliar = prediction(prediccion, DepositosVal$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo5_val = as.numeric(auc.tmp@y.values)


Modelo_1=c(auc_modelo1_train,auc_modelo1_val)
Modelo_2=c(auc_modelo2_train,auc_modelo2_val)
Modelo_3=c(auc_modelo3_train,auc_modelo3_val)
Modelo_4=c(auc_modelo4_train,auc_modelo4_val)
Modelo_5=c(auc_modelo5_train,auc_modelo5_val)

A=data.frame(Modelo_1,Modelo_2,Modelo_3,Modelo_4,Modelo_5)
rownames(A)=c("auc_train","auc_val")
colnames(A)=c("Modelo1","Modelo2","Modelo3","Modelo4","Modelo5")
A


##### 12. Bloque de curva ROC de modelo seleccionado #####

DepositosTrain$prediccion=predict(modelo4,type="response")
Pred_auxiliar= prediction(DepositosTrain$prediccion, DepositosTrain$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo4_train = as.numeric(auc.tmp@y.values)
auc_modelo4_train

CURVA_ROC_modelo4_train <- performance(Pred_auxiliar,"tpr","fpr")
plot(CURVA_ROC_modelo4_train,colorize=TRUE)
abline(a=0,b=1)

DepositosTest$prediccion=predict(modelo4, newdata=DepositosTest,type="response")
Pred_auxiliar = prediction(DepositosTest$prediccion, DepositosTest$CAMP_DEPOSITOS, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_modelo4_test = as.numeric(auc.tmp@y.values)
auc_modelo4_test

CURVA_ROC_modelo4_test <- performance(Pred_auxiliar,"tpr","fpr")
plot(CURVA_ROC_modelo4_test,colorize=TRUE)
abline(a=0,b=1)

##### 13. Bloque de capacidad del modelo #####
mean(as.numeric(DepositosTest$CAMP_DEPOSITOS)-1)
aggregate(DepositosTest$prediccion~DepositosTest$CAMP_DEPOSITOS,FUN=mean)

## -------------------------------------------------------------------------
##       PARTE 3: PUESTA EN VALOR DEL MODELO EXPLOTACIÓN
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 14. Bloque de puesta en valor de un modelo: Fijación del Threshold #####

ALPHA=0.5
Confusion_Test=table(DepositosTest$CAMP_DEPOSITOS,DepositosTest$prediccion>=ALPHA)
Accuracy_Test= (sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)+sum(DepositosTest$CAMP_DEPOSITOS==0 & DepositosTest$prediccion<ALPHA))/length(DepositosTest$CAMP_DEPOSITOS)
Precision_Test=sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)/sum(DepositosTest$prediccion>=ALPHA)
Cobertura_Test=sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)/sum(DepositosTest$CAMP_DEPOSITOS==1)
Confusion_Test
Accuracy_Test
Precision_Test
Cobertura_Test

ALPHA=0.2
Confusion_Test=table(DepositosTest$CAMP_DEPOSITOS,DepositosTest$prediccion>=ALPHA)
Accuracy_Test= (sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)+sum(DepositosTest$CAMP_DEPOSITOS==0 & DepositosTest$prediccion<ALPHA))/length(DepositosTest$CAMP_DEPOSITOS)
Precision_Test=sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)/sum(DepositosTest$prediccion>=ALPHA)
Cobertura_Test=sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)/sum(DepositosTest$CAMP_DEPOSITOS==1)
Confusion_Test
Accuracy_Test
Precision_Test
Cobertura_Test

ALPHA=0.8
Confusion_Test=table(DepositosTest$CAMP_DEPOSITOS,DepositosTest$prediccion>=ALPHA)
Accuracy_Test= (sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)+sum(DepositosTest$CAMP_DEPOSITOS==0 & DepositosTest$prediccion<ALPHA))/length(DepositosTest$CAMP_DEPOSITOS)
Precision_Test=sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)/sum(DepositosTest$prediccion>=ALPHA)
Cobertura_Test=sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)/sum(DepositosTest$CAMP_DEPOSITOS==1)
Confusion_Test
Accuracy_Test
Precision_Test
Cobertura_Test

## -------------------------------------------------------------------------

##### 15. Bloque de puesta en valor de un modelo: KS y punto de máxima separación #####

Depositos_KS=DepositosTest[order(DepositosTest$prediccion, decreasing=TRUE),c("CAMP_DEPOSITOS","prediccion")]
Depositos_KS$N=1:length(Depositos_KS$CAMP_DEPOSITOS)
Depositos_KS$EXITOS_ACUM=cumsum(as.numeric(Depositos_KS$CAMP_DEPOSITOS)-1)
Depositos_KS$FRACASOS_ACUM=Depositos_KS$N-Depositos_KS$EXITOS_ACUM
Depositos_KS$EXITOS_TOT=sum(Depositos_KS$CAMP_DEPOSITOS==1)
Depositos_KS$FRACASOS_TOT=sum(Depositos_KS$CAMP_DEPOSITOS==0)
Depositos_KS$TOTAL=Depositos_KS$EXITOS_TOT+Depositos_KS$FRACASOS_TOT
Depositos_KS$TPR=Depositos_KS$EXITOS_ACUM/Depositos_KS$EXITOS_TOT
Depositos_KS$FPR=Depositos_KS$FRACASOS_ACUM/Depositos_KS$FRACASOS_TOT
Depositos_KS$DIFF=Depositos_KS$TPR-Depositos_KS$FPR
plot(Depositos_KS$DIFF)
max(Depositos_KS$DIFF)
which(Depositos_KS$DIFF==max(Depositos_KS$DIFF))
Depositos_KS[5095,]

plot(Depositos_KS$prediccion*1000,1-Depositos_KS$TPR,xlab="SCORE",ylab="Porcentaje acumulado",main="Distribuciones por Score (rojo malos, azul buenos)",type="l",col="blue")
lines(Depositos_KS$prediccion*1000,1-Depositos_KS$FPR,col="red")

## -------------------------------------------------------------------------

##### 16. Bloque de puesta en valor de un modelo: F1Score y punto óptimo estadístico #####

Depositos_KS$Accuracy=(Depositos_KS$EXITOS_ACUM+Depositos_KS$FRACASOS_TOT-Depositos_KS$FRACASOS_ACUM)/Depositos_KS$TOTAL
Depositos_KS$Precision=Depositos_KS$EXITOS_ACUM/Depositos_KS$N
Depositos_KS$Cobertura=Depositos_KS$EXITOS_ACUM/Depositos_KS$EXITOS_TOT
Depositos_KS$F1Score=2*(Depositos_KS$Precision*Depositos_KS$Cobertura)/(Depositos_KS$Precision+Depositos_KS$Cobertura)
plot(Depositos_KS$F1Score)
max(Depositos_KS$F1Score)
which(Depositos_KS$F1Score==max(Depositos_KS$F1Score))
Depositos_KS[8511,]

## -------------------------------------------------------------------------

##### 17. Bloque de puesta en valor de un modelo: Beneficio y punto óptimo financiero #####

costeLlamada=10
beneficioVenta=15

Depositos_KS$BeneficioTP=beneficioVenta-costeLlamada
Depositos_KS$BeneficioTN=0
Depositos_KS$PerdidaFP=-costeLlamada
Depositos_KS$PerdidaFN=-beneficioVenta

Depositos_KS$BeneficioFinan=Depositos_KS$EXITOS_ACUM*Depositos_KS$BeneficioTP+
  Depositos_KS$FRACASOS_ACUM*Depositos_KS$PerdidaFP

Depositos_KS$Oportunidad=Depositos_KS$EXITOS_ACUM*Depositos_KS$BeneficioTP+
  (Depositos_KS$EXITOS_TOT-Depositos_KS$EXITOS_ACUM)*Depositos_KS$PerdidaFN+
  Depositos_KS$FRACASOS_ACUM*Depositos_KS$PerdidaFP+
  (Depositos_KS$FRACASOS_TOT-Depositos_KS$FRACASOS_ACUM)*Depositos_KS$BeneficioTN

plot(Depositos_KS$BeneficioFinan)
max(Depositos_KS$BeneficioFinan)
which(Depositos_KS$BeneficioFinan==max(Depositos_KS$BeneficioFinan))
Depositos_KS[5089,]

plot(Depositos_KS$Oportunidad)
max(Depositos_KS$Oportunidad)
which(Depositos_KS$Oportunidad==max(Depositos_KS$Oportunidad))
Depositos_KS[9253,]

## -------------------------------------------------------------------------

##### 18. Bloque de puesta en valor de un modelo: Tabla de resultados #####

Alphas=seq(from=0, to=1,by=0.05)
v_Accuracy=c()
v_Precision=c()
v_Cobertura=c()

for (i in 1:length(Alphas)){
  ALPHA=ALPHAS[i]
  Accuracy_Test= (sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)+sum(DepositosTest$CAMP_DEPOSITOS==0 & DepositosTest$prediccion<ALPHA))/length(DepositosTest$CAMP_DEPOSITOS)
  Precision_Test=sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)/sum(DepositosTest$prediccion>=ALPHA)
  Cobertura_Test=sum(DepositosTest$CAMP_DEPOSITOS==1 & DepositosTest$prediccion>=ALPHA)/sum(DepositosTest$CAMP_DEPOSITOS==1)
  v_Accuracy=c(v_Accuracy,Accuracy_Test)
  v_Precision=c(v_Precision,Precision_Test)
  v_Cobertura=c(v_Cobertura,Cobertura_Test)
}

TABLA_METRICAS=data.frame(Alphas,Acierto=v_Accuracy,Precision=v_Precision,Cobertura=v_Cobertura )
TABLA_METRICAS


Depositos_KS$Cuantiles=ntile(Depositos_KS$prediccion, 12)
TABLA_RESULTADOS=summarise(group_by(Depositos_KS,Cuantiles),
                 Clientes=sum(Cuantiles>0),
                 Compradores=sum(CAMP_DEPOSITOS==1),
                 PorcCompradores = sum(CAMP_DEPOSITOS==1)/sum(Cuantiles>0),
                 NoCompradores=sum(CAMP_DEPOSITOS==0),
                 PorcNoCompradores = sum(CAMP_DEPOSITOS==0)/sum(Cuantiles>0)
)

TABLA_RESULTADOS

## -----------------------------------------------------------------------