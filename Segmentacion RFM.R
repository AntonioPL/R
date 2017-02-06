## -------------------------------------------------------------------------
## SCRIPT: Segmentacion RFM.R
## FECHA: 30/09/2016
## MASTER: Master en Data Science en Kschool
## PROFESOR: Antonio Pita Lozano
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias y conexion a Teradata #####

library(dplyr)

## -------------------------------------------------------------------------

##### 2. Bloque de parametros de entrada #####

## Se inicializa la carpeta de salida
setwd("C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R")

## -------------------------------------------------------------------------

##### 3. Bloque de extracción y preparación de datos #####

VENTAS=read.csv2("Operaciones.csv")
str(VENTAS)
summary(VENTAS)
head(VENTAS)
tail(VENTAS)
VENTAS$FECHA=as.Date(VENTAS$FECHA)
VENTAS$FRECUENCIA=1
summary(VENTAS)

## -------------------------------------------------------------------------

##### 4. Bloque de construcción de variables RFM #####

FECHA_1=as.Date("2014-12-31")
FECHA_2=as.Date("2015-03-31")
FECHA_3=as.Date("2015-06-30")

RFM_VENTAS_1=summarise(group_by(VENTAS[VENTAS$FECHA<FECHA_1 & VENTAS$FECHA>=FECHA_1-365,], CLIENTE),
          RECENCIA = as.numeric(min(FECHA_1-FECHA, na.rm = TRUE)),
          FRECUENCIA = sum(FRECUENCIA, na.rm = TRUE),
          MONETIZACION =  sum(IMPORTE, na.rm = TRUE)
)
RFM_VENTAS_2=summarise(group_by(VENTAS[VENTAS$FECHA<FECHA_2 & VENTAS$FECHA>=FECHA_2-365,], CLIENTE),
                     RECENCIA = as.numeric(min(FECHA_2-FECHA, na.rm = TRUE)),
                     FRECUENCIA = sum(FRECUENCIA, na.rm = TRUE),
                     MONETIZACION =  sum(IMPORTE, na.rm = TRUE)
)
RFM_VENTAS_3=summarise(group_by(VENTAS[VENTAS$FECHA<FECHA_3 & VENTAS$FECHA>=FECHA_3-365,], CLIENTE),
                     RECENCIA = as.numeric(min(FECHA_3-FECHA, na.rm = TRUE)),
                     FRECUENCIA = sum(FRECUENCIA, na.rm = TRUE),
                     MONETIZACION =  sum(IMPORTE, na.rm = TRUE)
)


## -------------------------------------------------------------------------

##### 5. Bloque Gráfico de Densidad Modelo RFM 12M #####


png("./100.Grafico Densidad Modelo RFM 12M.png",width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
smoothScatter(RFM_VENTAS_1$FRECUENCIA,RFM_VENTAS_1$RECENCIA, xlab="FRECUENCIA", ylab="RECENCIA")
frame()
smoothScatter(RFM_VENTAS_1$FRECUENCIA,RFM_VENTAS_1$MONETIZACION, xlab="FRECUENCIA",ylab="MONETIZACION")
smoothScatter(RFM_VENTAS_1$RECENCIA,RFM_VENTAS_1$MONETIZACION, xlab="RECENCIA",ylab="MONETIZACION")
mtext("Densidad de clientes mediante Modelo RFM 12 meses ", outer = TRUE, cex = 2)
dev.off()

## -------------------------------------------------------------------------

##### 6. Bloque de Segmentación mediante Modelo RFM 12M  #####

## PREPARAMOS LOS DATOS PARA LA SEGMENTACIÓN MEDIANTE SU NORMALIZACIÓN
RFM_VENTAS_1_NORM=scale(RFM_VENTAS_1[,-1])

## CALCULAMOS LOS SEGMENTOS EN FUNCIÓN AL NÚMERO ELEGIDO
NUM_CLUSTERS=8
set.seed(1234)
Modelo=kmeans(RFM_VENTAS_1_NORM,NUM_CLUSTERS)

## SELECCIONAMOS LOS GRUPOS
Segmentos=Modelo$cluster

## MOSTRAMOS LA DISTRIBUCIÓN DE LOS GRUPOS
table(Segmentos)

## MOSTRAMOS LOS DATOS REPRESENTATIVOS DE LOS GRUPOS
aggregate(RFM_VENTAS_1[,-1], by = list(Segmentos), mean)


## CENTROS

SEGMENTOS=aggregate(RFM_VENTAS_1[,-1], by = list(Segmentos), mean)
SEGMENTOS$CONTADOR=table(Segmentos)
NORMALIZACION_MEDIA=apply(RFM_VENTAS_1[,-1],MARGIN=2,FUN=mean)
NORMALIZACION_SD=apply(RFM_VENTAS_1[,-1],MARGIN=2,FUN=sd)

## Comprobación de los centroides construidos
Modelo$centers[,1]*NORMALIZACION_SD[1]+NORMALIZACION_MEDIA[1]
SEGMENTOS$RECENCIA

## -------------------------------------------------------------------------

##### 7. Bloque de Representación Gráfica Segmentación mediante Modelo RFM 12M #####

dev.off()

png(paste("./101.Grafico Clustering Kmeans para ",NUM_CLUSTERS," CLUSTERS del Modelo RFM 12M.png",sep=""),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
plot(RFM_VENTAS_1$FRECUENCIA,RFM_VENTAS_1$RECENCIA,col=Segmentos, xlab="FRECUENCIA", ylab="RECENCIA")
plot(c(0,max(RFM_VENTAS_1$RECENCIA)),c(0,max(RFM_VENTAS_1$RECENCIA)), type="n", axes=F, xlab="", ylab="",xlim=c(0,max(RFM_VENTAS_1$RECENCIA)),ylim=c(0,max(RFM_VENTAS_1$RECENCIA)))
legend(1,max(RFM_VENTAS_1$RECENCIA)/2-1,legend=c(1:NUM_CLUSTERS),yjust = 0.5,col=c(1:NUM_CLUSTERS),pch=15,cex=2)
plot(RFM_VENTAS_1$FRECUENCIA,RFM_VENTAS_1$MONETIZACION,col=Segmentos, xlab="FRECUENCIA",ylab="MONETIZACION")
plot(RFM_VENTAS_1$RECENCIA,RFM_VENTAS_1$MONETIZACION,col=Segmentos, xlab="RECENCIA",ylab="MONETIZACION")
mtext(paste("Clusterización kmeans de clientes mediante Modelo RFM 12 meses",sep=""), outer = TRUE, cex = 2)
dev.off()


## -------------------------------------------------------------------------

##### 8. Bloque de Resultados Segmentación mediante Modelo RFM 12M exportados a Excel #####

RFM_VENTAS_1$SEGMENTO_1=Segmentos
write.csv2(SEGMENTOS,file=paste("104. Resumen de la segmentación kmeans con ",NUM_CLUSTERS," clusters del Modelo RFM 12M.csv",sep=""),row.names=FALSE)


## -------------------------------------------------------------------------
##### 9. Bloque de Representación Gráfica Segmentación mediante Modelo RFM 12M sin conjuntos Outliers#####

## HAY QUE INDICAR EL SEGMENTO DE OUTLIERS
OUTLIERS=c(2,4)

RFM_VENTAS_1_X=RFM_VENTAS_1[!(Segmentos %in% OUTLIERS),]
Segmentos_X=Segmentos[!(Segmentos %in% OUTLIERS)]
dev.off()

png(paste("./102.Zoom Grafico Clustering X Kmeans para ",NUM_CLUSTERS," CLUSTERS del Modelo RFM 12M.png",sep=""),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
plot(RFM_VENTAS_1_X$FRECUENCIA,RFM_VENTAS_1_X$RECENCIA,col=Segmentos_X, xlab="FRECUENCIA", ylab="RECENCIA")
plot(c(0,max(RFM_VENTAS_1_X$RECENCIA)),c(0,max(RFM_VENTAS_1_X$RECENCIA)), type="n", axes=F, xlab="", ylab="",xlim=c(0,max(RFM_VENTAS_1_X$RECENCIA)),ylim=c(0,max(RFM_VENTAS_1_X$RECENCIA)))
legend(1,max(RFM_VENTAS_1_X$RECENCIA)/2-1,legend=c(1:NUM_CLUSTERS),yjust = 0.5,col=c(1:NUM_CLUSTERS),pch=15,cex=2)
plot(RFM_VENTAS_1_X$FRECUENCIA,RFM_VENTAS_1_X$MONETIZACION,col=Segmentos_X, xlab="FRECUENCIA",ylab="MONETIZACION")
plot(RFM_VENTAS_1_X$RECENCIA,RFM_VENTAS_1_X$MONETIZACION,col=Segmentos_X, xlab="RECENCIA",ylab="MONETIZACION")
mtext(paste("Clusterización kmeans de clientes mediante Modelo RFM 12 meses",sep=""), outer = TRUE, cex = 2)
dev.off()


## -------------------------------------------------------------------------
##### 10. Bloque de Representación Gráfica Centroides Segmentación mediante Modelo RFM 12M sin conjuntos Outliers#####

png(paste("./103.Grafico Centroides Clustering X Kmeans para ",NUM_CLUSTERS," CLUSTERS del Modelo RFM 12M.png",sep=""),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
plot(SEGMENTOS$FRECUENCIA,SEGMENTOS$RECENCIA,col=SEGMENTOS$Group.1, xlab="FRECUENCIA", ylab="RECENCIA",pch=15,cex=2)
plot(c(0,max(SEGMENTOS$RECENCIA)),c(0,max(SEGMENTOS$RECENCIA)), type="n", axes=F, xlab="", ylab="",xlim=c(0,max(SEGMENTOS$RECENCIA)),ylim=c(0,max(SEGMENTOS$RECENCIA)))
legend(1,max(SEGMENTOS$RECENCIA)/2-1,legend=c(1:NUM_CLUSTERS),yjust = 0.5,col=c(1:NUM_CLUSTERS),pch=15,cex=2)
plot(SEGMENTOS$FRECUENCIA,SEGMENTOS$MONETIZACION,col=SEGMENTOS$Group.1, xlab="FRECUENCIA",ylab="MONETIZACION",pch=15,cex=2)
plot(SEGMENTOS$RECENCIA,SEGMENTOS$MONETIZACION,col=SEGMENTOS$Group.1, xlab="RECENCIA",ylab="MONETIZACION",pch=15,cex=2)
mtext(paste("Clusterización kmeans de clientes mediante Modelo RFM 12 meses",sep=""), outer = TRUE, cex = 2)
dev.off()


## -------------------------------------------------------------------------
##### 11. Bloque de Cálculo de Centroides para el segundo periodo #####

RFM_VENTAS_2$DIST_CLUSTER_1=((RFM_VENTAS_2$RECENCIA-SEGMENTOS$RECENCIA[1])/NORMALIZACION_SD[1])^2+((RFM_VENTAS_2$FRECUENCIA-SEGMENTOS$FRECUENCIA[1])/NORMALIZACION_SD[2])^2+((RFM_VENTAS_2$MONETIZACION-SEGMENTOS$MONETIZACION[1])/NORMALIZACION_SD[3])^2
RFM_VENTAS_2$DIST_CLUSTER_2=((RFM_VENTAS_2$RECENCIA-SEGMENTOS$RECENCIA[2])/NORMALIZACION_SD[1])^2+((RFM_VENTAS_2$FRECUENCIA-SEGMENTOS$FRECUENCIA[2])/NORMALIZACION_SD[2])^2+((RFM_VENTAS_2$MONETIZACION-SEGMENTOS$MONETIZACION[2])/NORMALIZACION_SD[3])^2
RFM_VENTAS_2$DIST_CLUSTER_3=((RFM_VENTAS_2$RECENCIA-SEGMENTOS$RECENCIA[3])/NORMALIZACION_SD[1])^2+((RFM_VENTAS_2$FRECUENCIA-SEGMENTOS$FRECUENCIA[3])/NORMALIZACION_SD[2])^2+((RFM_VENTAS_2$MONETIZACION-SEGMENTOS$MONETIZACION[3])/NORMALIZACION_SD[3])^2
RFM_VENTAS_2$DIST_CLUSTER_4=((RFM_VENTAS_2$RECENCIA-SEGMENTOS$RECENCIA[4])/NORMALIZACION_SD[1])^2+((RFM_VENTAS_2$FRECUENCIA-SEGMENTOS$FRECUENCIA[4])/NORMALIZACION_SD[2])^2+((RFM_VENTAS_2$MONETIZACION-SEGMENTOS$MONETIZACION[4])/NORMALIZACION_SD[3])^2
RFM_VENTAS_2$DIST_CLUSTER_5=((RFM_VENTAS_2$RECENCIA-SEGMENTOS$RECENCIA[5])/NORMALIZACION_SD[1])^2+((RFM_VENTAS_2$FRECUENCIA-SEGMENTOS$FRECUENCIA[5])/NORMALIZACION_SD[2])^2+((RFM_VENTAS_2$MONETIZACION-SEGMENTOS$MONETIZACION[5])/NORMALIZACION_SD[3])^2
RFM_VENTAS_2$DIST_CLUSTER_6=((RFM_VENTAS_2$RECENCIA-SEGMENTOS$RECENCIA[6])/NORMALIZACION_SD[1])^2+((RFM_VENTAS_2$FRECUENCIA-SEGMENTOS$FRECUENCIA[6])/NORMALIZACION_SD[2])^2+((RFM_VENTAS_2$MONETIZACION-SEGMENTOS$MONETIZACION[6])/NORMALIZACION_SD[3])^2
RFM_VENTAS_2$DIST_CLUSTER_7=((RFM_VENTAS_2$RECENCIA-SEGMENTOS$RECENCIA[7])/NORMALIZACION_SD[1])^2+((RFM_VENTAS_2$FRECUENCIA-SEGMENTOS$FRECUENCIA[7])/NORMALIZACION_SD[2])^2+((RFM_VENTAS_2$MONETIZACION-SEGMENTOS$MONETIZACION[7])/NORMALIZACION_SD[3])^2
RFM_VENTAS_2$DIST_CLUSTER_8=((RFM_VENTAS_2$RECENCIA-SEGMENTOS$RECENCIA[8])/NORMALIZACION_SD[1])^2+((RFM_VENTAS_2$FRECUENCIA-SEGMENTOS$FRECUENCIA[8])/NORMALIZACION_SD[2])^2+((RFM_VENTAS_2$MONETIZACION-SEGMENTOS$MONETIZACION[8])/NORMALIZACION_SD[3])^2

RFM_VENTAS_2$minimo=apply(RFM_VENTAS_2[,5:(4+NUM_CLUSTERS)],MARGIN=1,FUN=min,na.rm=TRUE)

RFM_VENTAS_2_AUX=RFM_VENTAS_2[,5:(4+NUM_CLUSTERS)]==RFM_VENTAS_2$minimo
RFM_VENTAS_2$SEGMENTO_2=apply(RFM_VENTAS_2_AUX,MARGIN=1,FUN=which)

table(RFM_VENTAS_1$SEGMENTO_1)
table(RFM_VENTAS_2$SEGMENTO_2)

## -------------------------------------------------------------------------
##### 12. Bloque de Matriz de Transición #####

RFM_VENTAS_1_2=merge(RFM_VENTAS_1[,c("CLIENTE", "SEGMENTO_1")],RFM_VENTAS_2[,c("CLIENTE", "SEGMENTO_2")],all.x=TRUE,all.y=TRUE)
RFM_VENTAS_1_2$SEGMENTO_1[is.na(RFM_VENTAS_1_2$SEGMENTO_1)]=0
RFM_VENTAS_1_2$SEGMENTO_2[is.na(RFM_VENTAS_1_2$SEGMENTO_2)]=0

table(RFM_VENTAS_1_2$SEGMENTO_1,RFM_VENTAS_1_2$SEGMENTO_2)

## -------------------------------------------------------------------------