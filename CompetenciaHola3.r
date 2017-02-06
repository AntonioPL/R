#font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(reshape)
library(jpeg)
library(extrafont)
library(DT)

set.seed(42)
PibEspana <- 22685
# carga de ficheros, datos de población,ventas,economÃ?a,pv
#
dfpob_prev  <- read.csv(file="C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/poblacionprov.csv",sep=";",header=T)
dfdata <- read.csv(file="C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/ventacompetencia.csv",sep=";",header=T)
dfecon <- read.csv(file="C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/economia.csv",sep=";",header=T)
dfpv <- read.csv(file="C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/puntosventa.csv",sep=";",header=T)


dfdata$Publicacion <- gsub(' ','',dfdata$Publicacion)
dfdata$Formato <- gsub(' ','',dfdata$Formato)

dfpob <- melt.data.frame(dfpob_prev)

dfpob$variable <- gsub('X','',dfpob$variable)

dfpob$variable <- as.factor(dfpob$variable)
dfdata$Año <- as.factor(dfdata$Año)

#union de los dataframes
#
mixto0 <- merge(dfpob,dfdata,by.x =c("PLAZA","variable") , by.y = c("PLAZA","Año"),sort=F)
mixto1<-  merge(mixto0,dfecon,by.x ="PLAZA" , by.y = "PLAZA",sort=F)
mixto <-  merge(mixto1,dfpv,by.x ="PLAZA" , by.y = "PLAZA",sort=F)
head(mixto)

mixto$Publicacion <- gsub('DIEZMINUTOS','DIEZ MINUTOS',mixto$Publicacion)


mixto$Publicacion_Formato <- paste(mixto$Publicacion,mixto$Formato)
mixto$Penetracion <- mixto$Venta/mixto$value

# funcion para ordenar las provincias por penetración y que salgan ordenadas en el grÃ¡fico
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

#######################################################################
# Venta Total Nacional (Formato Grande y Pequeño)
#######################################################################
# Agrupacion por Año y publicación
mixtogroup <- group_by(mixto, Publicacion,variable)
df1 <- summarise(mixtogroup,
                   Venta = sum(Venta, na.rm = TRUE))

g1 <- ggplot(df1,aes(x=variable , y= Venta , group=Publicacion))+
  #geom_point(col="black",size=3)+
  geom_line(aes(col=Publicacion),size=1.5)+
  geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) + 
  scale_color_manual(name = "Publicación",values=c("dark green","red","blue","black"))+
  labs(title = "Venta Media Nacional (Formato Grande y Pequeño)",x="Año",y="Ejemplares Vendidos (OJD)") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0.5,face="plain", size=rel(1.5))) +
  theme(legend.text  = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_discrete( labels = scales::comma,breaks = c("2013","2014","2015","2016"))+
  scale_y_continuous(limits = c(0, 350000), labels = scales::comma,breaks = c(0,50000,100000,150000,200000,250000,300000,350000))
g1

#######################################################################
#Grafico Anual por publicación y tamAño suma de todos los formatos
#######################################################################

mixtogroup2 <- group_by(mixto, variable, Publicacion_Formato)
df2 <- summarise(mixtogroup2,
                 Venta = sum(Venta, na.rm = TRUE))

g2 <- ggplot(df2,aes(x=variable , y= Venta , group=Publicacion_Formato))+geom_point(col="black",size=2)+
  geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) + 
  geom_line(aes(col=Publicacion_Formato ,linetype=Publicacion_Formato),size=1.5 )+
  scale_linetype_manual(name = "Publicación y formato",values=c("solid","dotted","solid","solid","dotted","solid","dotted")) +
  scale_color_manual(name = "Publicación y formato",values=c("dark green","dark green", "red","blue", "blue","black","black")) +
  labs(title = "Venta Media Nacional Publicación y Formato",x="Año",y="Ejemplares Vendidos (OJD)") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  #theme(legend.position="none") +
  scale_x_discrete( limits=c("2013","2014","2015","2016"), labels = scales::comma,breaks = seq(2013,2016,1))+
  scale_y_continuous(limits = c(0, 350000), labels = scales::comma,breaks = c(0,50000,100000,150000,200000,250000,300000,350000))
g2

#######################################################################
# Grafico de SEMANA
#######################################################################

df21 <- df2[grep("SEMANA",df2$Publicacion_Formato),]

g21 <- ggplot(df21,aes(x=variable , y= Venta, fill = Publicacion_Formato,label=Venta ))+
  geom_bar(stat = "identity",position="dodge") +
  geom_text(aes(label=format(Venta,big.mark=",")),colour="black", size=4.0,hjust=0.5,position=position_dodge(width=0.9), vjust=-0.25) +
  #geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) +
  labs(title = "Venta Media SEMANA por Formato",x="Año",y="Ejemplares Vendidos (OJD)") +
  scale_fill_manual("Formato", values = c("SEMANA GRANDE" = "black", "SEMANA PEQUEÃ‘O" = "dark grey")) +
  scale_linetype_manual(name = "Formato") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  #theme(legend.position="none") +
  scale_x_discrete( limits=c("2013","2014","2015","2016"), labels = scales::comma,breaks = seq(2013,2016,1))+
  scale_y_continuous(limits = c(0, 125000), labels = scales::comma,breaks = seq(0,125000,25000))
g21

#######################################################################
# Grafico de LECTURAS
#######################################################################

df22 <- df2[grep("LECTURAS",df2$Publicacion_Formato),]

g22 <- ggplot(df22,mapping= aes(x=variable , y= Venta,fill = Publicacion_Formato ))+
  geom_bar(stat = "identity",position="dodge") +
  geom_text(aes(label=format(Venta,big.mark=",")),colour="black", size=4.0,hjust=0.5,position=position_dodge(width=0.9), vjust=-0.25) +
  #geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) +
  labs(title = "Venta Total LECTURAS por Formato",x="Año",y="Ejemplares Vendidos (OJD)") +
  scale_fill_manual("Formato", values = c("LECTURAS GRANDE" = "dark blue", "LECTURAS PEQUEÃ‘O" = "blue")) +
  scale_linetype_manual(name = "Formato") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_discrete( limits=c("2013","2014","2015","2016"), labels = scales::comma,breaks = seq(2013,2016,1))+
  scale_y_continuous(limits = c(0, 175000), labels = scales::comma,breaks = seq(0,200000,25000))
g22

#######################################################################
# Grafico de DIEZMINUTOS
#######################################################################

df23 <- df2[grep("DIEZ MINUTOS",df2$Publicacion_Formato),]

g23 <- ggplot(df23,mapping= aes(x=variable , y= Venta,fill = Publicacion_Formato ))+
  geom_bar(stat = "identity",position="dodge")+
  geom_text(aes(label=format(Venta,big.mark=",")),colour="black", size=4.0,hjust=0.5,position=position_dodge(width=0.9), vjust=-0.25) +
  #geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) +
  labs(title = "Venta Total DIEZ MINUTOS por Formato",x="Año",y="Ejemplares Vendidos (OJD)") +
  scale_fill_manual("Formato", values = c("DIEZ MINUTOS GRANDE" = "dark green", "DIEZ MINUTOS PEQUEÃ‘O" = "green")) +
  scale_linetype_manual(name = "Formato") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_discrete( limits=c("2013","2014","2015","2016"), labels = scales::comma,breaks = seq(2013,2016,1))+
  scale_y_continuous(limits = c(0, 225000), labels = scales::comma,breaks = seq(0,225000,25000))
g23

#######################################################################
# Grafico de HOLA
#######################################################################

df24 <- df2[grep("HOLA",df2$Publicacion_Formato),]

g24 <- ggplot(df24,mapping= aes(x=variable , y= Venta,fill = Publicacion_Formato ))+
  geom_bar(stat = "identity",position="dodge")+
  geom_text(aes(label=format(Venta,big.mark=",")),colour="black", size=4.0,hjust=0.5,position=position_dodge(width=0.9), vjust=-0.25) +
  #geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) +
  labs(title = "Venta Total HOLA por Formato",x="Año",y="Ejemplares Vendidos (OJD)") +
  scale_fill_manual("Formato", values = c("HOLA GRANDE" = "red")) +
  scale_linetype_manual(name = "Formato") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_discrete( limits=c("2013","2014","2015","2016"), labels = scales::comma,breaks = seq(2013,2016,1))+
  scale_y_continuous(limits = c(0, 350000), labels = scales::comma,breaks = seq(0,350000,25000))
g24

#######################################################################
#Grafico Anual por publicación suma de todos los formatos
#######################################################################

mixtogroup3 <- group_by(mixto, PLAZA,variable)
df3 <- summarise(mixtogroup3,
                 Penetracion = sum(Penetracion, na.rm = TRUE))
# solo datos de 2016
df31 <- df3[df3$variable=="2016",]

df31 <- df31[ order(df31$Penetracion), ]

g3 <- ggplot(df31,aes( x=PLAZA, y=Penetracion  ,label = paste0(format(100*Penetracion,digits=2),"%")))+
  geom_bar(stat = "identity",fill='light blue')+
  #geom_text_repel(force=0.1,colour="black", segment.colour="black", size=5.0) +
  geom_text(hjust=0.5,colour="black",  size=4.0,check_overlap = F) +
  labs(title = "Penetración Total por Provincia (2016)",x="Provincia",y="Penetración total") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0.5,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  #scale_x_discrete(labels = NULL,limit = reorder_size(df31$PLAZA)) +
  scale_x_discrete(limit = reorder_size(df31$PLAZA)) +
  scale_y_continuous(labels = scales::percent)
g3


#######################################################################
# Grafico Penetracion total por Comunidad
#######################################################################

numpub <-length(unique(mixto$Publicacion_Formato))

mixtogroup41 <- group_by(mixto, COMUNIDAD,variable)

df41 <- summarise(mixtogroup41,
                  Penetracion = sum(Penetracion, na.rm = TRUE)*numpub/length(PLAZA))
#  ñapa total para sacar la penetración por provincia sacando el nÃºpero de provincias

g41 <- ggplot(df41,aes(x=variable , y= Penetracion ,group=COMUNIDAD))+geom_point(col="black",size=2) +
  geom_line(col="orange",size=1.5) + 
  labs(title = "Penetración Total por Comunidad y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ COMUNIDAD)+facet_wrap(~ COMUNIDAD, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0))
g41

# solo datos de 2016
df42 <- df41[df41$variable=="2016",]

df42 <- df42[ order(df42$Penetracion), ]

g42 <- ggplot(df42,aes( x=COMUNIDAD, y=Penetracion,label = paste0(format(100*Penetracion,digits=2),"%")))+
  geom_bar(stat = "identity",fill='light blue') +
  geom_text(hjust=0.5,colour="black",  size=5.0) +
  labs(title = "Penetración Total por Comunidad (2016)",x="Provincia",y="Penetración total") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_discrete(limit = reorder_size(df42$COMUNIDAD)) +
  scale_y_continuous(labels = scales::percent)
g42
#######################################################################
# Grafico Penetracion total HOLA por Comunidad
#######################################################################

numpub <-length(unique(mixto$Publicacion_Formato))

mixtohola <- mixto[mixto$Publicacion=="HOLA",]

mixtogroupComHola <- group_by(mixtohola, COMUNIDAD,variable)

df41Hola <- summarise(mixtogroupComHola,
                  Penetracion = sum(Penetracion, na.rm = TRUE)/length(PLAZA))


g41Hola <- ggplot(df41Hola,aes(x=variable , y= Penetracion ,group=COMUNIDAD))+geom_point(col="black",size=2) +
  geom_line(col="red",size=1.5) + 
  labs(title = "Penetración HOLA por Comunidad y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(hjust = 0.5,face="bold", size=rel(2))) +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ COMUNIDAD)+facet_wrap(~ COMUNIDAD, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0))
g41Hola

#######################################################################
# Grafico Penetracion total LECTURAS por Comunidad
#######################################################################
mixtolecturas <- mixto[mixto$Publicacion=="LECTURAS",]
mixtogroupComlecturas <- group_by(mixtolecturas, COMUNIDAD,variable)

df41Lecturas <- summarise(mixtogroupComlecturas,
                          Penetracion = sum(Penetracion, na.rm = TRUE)/length(PLAZA)*2) #por los 2 formatos

g41Lecturas <- ggplot(df41Lecturas,aes(x=variable , y= Penetracion ,group=COMUNIDAD))+geom_point(col="black",size=2) +
  geom_line(col="blue",size=1.5) + 
  labs(title = "Penetración LECTURAS por Comunidad y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ COMUNIDAD)+facet_wrap(~ COMUNIDAD, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0))
g41Lecturas
#######################################################################
# Grafico Penetracion total SEMANA por Comunidad
#######################################################################
mixtodiezminutos <- mixto[mixto$Publicacion=="DIEZ MINUTOS",]
mixtogroupComDiezMinutos <- group_by(mixtodiezminutos, COMUNIDAD,variable)

df41DiezMinutos <- summarise(mixtogroupComDiezMinutos,
                        Penetracion = sum(Penetracion, na.rm = TRUE)/length(PLAZA)*2) #por los 2 formatos

g41DiezMinutos <- ggplot(df41DiezMinutos,aes(x=variable , y= Penetracion ,group=COMUNIDAD))+geom_point(col="black",size=2) +
  geom_line(col="dark green",size=1.5) + 
  labs(title = "Penetración DIEZ MINUTOS por Comunidad y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ COMUNIDAD)+facet_wrap(~ COMUNIDAD, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0))
g41DiezMinutos
#######################################################################
# Grafico Penetracion total SEMANA por Comunidad
#######################################################################
mixtosemana <- mixto[mixto$Publicacion=="SEMANA",]
mixtogroupComSemana <- group_by(mixtosemana, COMUNIDAD,variable)

df41Semana <- summarise(mixtogroupComSemana,
                        Penetracion = sum(Penetracion, na.rm = TRUE)/length(PLAZA)*2) #por los 2 formatos


g41Semana <- ggplot(df41Semana,aes(x=variable , y= Penetracion ,group=COMUNIDAD))+geom_point(col="black",size=2) +
  geom_line(col="black",size=1.5) + 
  labs(title = "Penetración SEMANA por Comunidad y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ COMUNIDAD)+facet_wrap(~ COMUNIDAD, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0))
g41Semana
#######################################################################
# Grafico Penetracion total por provincia
#######################################################################

mixtogroup5 <- group_by(mixto, PLAZA,variable)
df5 <- summarise(mixtogroup5,
                 Penetracion = sum(Penetracion, na.rm = TRUE))

g5 <- ggplot(df5,aes(x=variable , y= Penetracion ,group=PLAZA))+geom_point(col="black",size=2) +
  geom_line(col="orange",size=1.5) + 
  labs(title = "Penetración Total por Provincia y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ PLAZA)+facet_wrap(~ PLAZA, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 8, colour = "black", angle = 0))
g5
#######################################################################
# Grafico Penetracion HOLA por provincia
#######################################################################
mixtogroup5hola <- group_by(mixto, variable,PLAZA,Publicacion)
df50 <- summarise(mixtogroup5hola,
                 Penetracion = sum(Penetracion, na.rm = TRUE))

df5Hola <- df50[df50$Publicacion=="HOLA",]
g5Hola <- ggplot(df5Hola,aes(x=variable , y= Penetracion ,group=PLAZA))+geom_point(col="black",size=2) +
  geom_line(col="red",size=1.5) + 
  labs(title = "Penetración Total HOLA por Provincia y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ PLAZA)+facet_wrap(~ PLAZA, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 8, colour = "black", angle = 0))
g5Hola
#######################################################################
# Grafico Penetracion LECTURAS por provincia
#######################################################################
df5Lecturas <- df50[df50$Publicacion=="LECTURAS",]
g5Lecturas <- ggplot(df5Lecturas,aes(x=variable , y= Penetracion ,group=PLAZA))+geom_point(col="black",size=2) +
  geom_line(col="blue",size=1.5) + 
  labs(title = "Penetración Total LECTURAS por Provincia y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ PLAZA)+facet_wrap(~ PLAZA, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 8, colour = "black", angle = 0))
g5Lecturas
#######################################################################
# Grafico Penetracion DIEZMINUTOS por provincia
#######################################################################
df5DiezMinutos <- df50[df50$Publicacion=="DIEZ MINUTOS",]
g5DiezMinutos <- ggplot(df5DiezMinutos,aes(x=variable , y= Penetracion ,group=PLAZA))+geom_point(col="black",size=2) +
  geom_line(col="dark green",size=1.5) + 
  labs(title = "Penetración Total DIEZ MINUTOS por Provincia y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ PLAZA)+facet_wrap(~ PLAZA, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 8, colour = "black", angle = 0))
g5DiezMinutos
#######################################################################
# Grafico Penetracion SEMANA por provincia
#######################################################################
df5Semana <- df50[df50$Publicacion=="SEMANA",]
g5Semana <- ggplot(df5Semana,aes(x=variable , y= Penetracion ,group=PLAZA))+geom_point(col="black",size=2) +
  geom_line(col="black",size=1.5) + 
  labs(title = "Penetración Total SEMANA por Provincia y Año",x="",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ PLAZA)+facet_wrap(~ PLAZA, nrow = 3,scales = "free_x")+
  theme(strip.text.x = element_text(size = 8, colour = "black", angle = 0))
g5Semana
#######################################################################
# GrÃ¡fico PÃºblico objetivo por Punto de Venta y plaza
#######################################################################
mixto$pvpob <- mixto$value/mixto$PuntosVenta
mixtogroup6 <- group_by(mixto, PLAZA,variable)
df6 <- summarise(mixtogroup6,
                 pvpob = mean(pvpob, na.rm = TRUE))
# solo datos de 2016
df61 <- df6[df6$variable=="2016",]
# reordenar
df61 <- df61[ order(df61$pvpob), ]

g6 <- ggplot(df61,aes( x=PLAZA, y=pvpob  ,label=round(pvpob)))+geom_bar(stat = "identity",fill='light blue')+
  #â—˜geom_text_repel(colour="black", segment.colour="black", size=4.0,force=0.1) +
  geom_text(colour="black", size=4.0,hjust=0.5) +
  labs(title = "PÃºblico objetivo por Punto de Venta y Provincia (2016)",x="Provincia",y="PÃºblico") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_discrete(limit = reorder_size(df61$PLAZA)) +
  scale_y_continuous(labels = scales::comma)
g6
#######################################################################
# GrÃ¡fico PÃºblico objetivo por Punto de Venta y COMUNIDAD
#######################################################################
mixtogroup62 <- group_by(mixto, COMUNIDAD,variable)
df62 <- summarise(mixtogroup62,
                 pv = sum (PuntosVenta, na.rm = TRUE),
                 sup = sum (Superficie, na.rm = TRUE),
                 Penetracion = sum (Penetracion, na.rm = TRUE),
                 ratio = sup/pv,
                 pvpob = sum(pvpob, na.rm = TRUE)/length(PLAZA))

# solo datos de 2016
df62 <- df62[df62$variable=="2016",]
# reordenar
df62 <- df62[ order(df62$pvpob), ]

g62 <- ggplot(df62,aes( x=COMUNIDAD, y=pvpob  ,label=round(pvpob)))+
  geom_bar(stat = "identity",fill='light blue')+
  geom_text(colour="black", size=4.0,hjust=0.5) +
  labs(title = "PÃºblico objetivo por Punto de Venta y Comunidad (2016)",x="PLAZA",y="PÃºblico") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_discrete(limit = reorder_size(df62$COMUNIDAD)) +
  scale_y_continuous(labels = scales::comma)
g62
#######################################################################
# Grafico Punto Venta por superficie PROVINCIAL
#######################################################################
mixto$pvsup <- mixto$PuntosVenta/mixto$Superficie
mixto$pvsup <- mixto$Superficie/mixto$PuntosVenta
mixtogroup7 <- group_by(mixto, PLAZA,variable)
df7 <- summarise(mixtogroup7,
                 pvsup = mean(pvsup, na.rm = TRUE))
# solo datos de 2016
df71 <- df7[df7$variable=="2016",]
# reordenar
df71 <- df71[ order(df71$pvsup), ]

g7 <- ggplot(df71,aes( x=PLAZA, y=pvsup  ,label=round(pvsup,2)))+geom_bar(stat = "identity",fill='light blue')+
  #geom_text_repel(colour="black", segment.colour="black", size=4.0,force=0.1) +
  geom_text(colour="black", size=4.0,hjust=0.5) +
  labs(title = "Puntos de Venta por Superficie y Provincia",x="PLAZA",y="Km2") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_discrete(limit = reorder_size(df71$PLAZA)) +
  scale_y_continuous(labels = scales::comma)
g7
#######################################################################
# Grafico Punto Venta por superficie Y Comunidad
#######################################################################
mixtogroup72 <- group_by(mixto, COMUNIDAD,variable)
df72 <- summarise(mixtogroup72,
              #    pvsup = sum(pvsup, na.rm = TRUE)*numpub/length(PLAZA))
              pvsup = sum(pvsup, na.rm = TRUE)/length(PLAZA))
              #pvsup = sum(pvsup, na.rm = TRUE))

# solo datos de 2016
df72 <- df72[df72$variable=="2016",]
# reordenar
df72 <- df72[ order(df72$pvsup), ]

g72 <- ggplot(df72,aes( x=COMUNIDAD, y=pvsup  ,label = round(pvsup,2)))+
  geom_bar(stat = "identity",fill='light blue')+
  #geom_text_repel(colour="black", segment.colour="black", size=5.0) +
  geom_text(colour="black", size=4.0,hjust=0.5) +
  labs(title = "Puntos de Venta por Km2",x="PLAZA",y="Puntos de Venta") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
scale_x_discrete(limit = reorder_size(df72$COMUNIDAD)) +
scale_y_continuous(labels = scales::comma)
g72
#######################################################################
# Grafico Penetracion Provincia y PIB
#######################################################################
mixto$pibtotal <- as.numeric(mixto$PIB)*as.numeric(mixto$value)
mixtogroup8 <- group_by(mixto, PLAZA,COMUNIDAD,variable)
df8 <- summarise(mixtogroup8,
                 PuntosVenta=sum(PuntosVenta, na.rm = TRUE),
                 Penetracion=sum(Penetracion, na.rm = TRUE),
                 pibtotal = as.numeric(sum(pibtotal, na.rm = TRUE)),
                 Pob = sum(value, na.rm = TRUE))
df8
df8$PibMed <- df8$pibtotal/df8$Pob/PibEspana

df81 <- df8[df8$variable=="2016",]
lr <- coef(lm(df81$Penetracion~df81$PibMed))

g8 <- ggplot(df81,aes( x=PibMed, y=Penetracion  ,label = PLAZA ))+
  geom_point(aes(color=COMUNIDAD),size = 2) + 
  scale_color_manual(values=c("red", "pink", "blue","lime green", "black", "salmon","dark grey", "green", "yellow",
                              "orange", "green", "dark green","light blue", "dark green", "light green","purple", "dark grey"))+
  geom_text_repel() +
  theme(panel.background = element_rect(fill = '#F2F2F2', colour = 'black')) + 
      geom_abline(intercept = lr[1], slope = lr[2] ) + 
      theme(panel.grid.major =  element_line(), panel.grid.minor = element_line())+
      labs(title = "Penetración Total por Comunidad y PIB (2016)",x="Pib Per CÃ¡pita Relativo",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::percent_format())
g8
#######################################################################
# Grafico Penetracion Comunidad y PIB
#######################################################################
#mixto$pibtotal <- as.numeric(mixto$PIB)*as.numeric(mixto$value)
mixtogroup9 <- group_by(mixto, COMUNIDAD,variable)
df9 <- summarise(mixtogroup9,
                 PuntosVenta=sum(PuntosVenta, na.rm = TRUE)/numpub,
                 Penetracion=sum(Penetracion, na.rm = TRUE)/length(PLAZA)*numpub,
                 pibtotal = as.numeric(sum(pibtotal, na.rm = TRUE)/numpub),
                 Pob = sum(value, na.rm = TRUE)/numpub)
df9
df9$PibMed <- df9$pibtotal/df9$Pob/PibEspana

df91 <- df9[df9$variable=="2016",]

lr2 <- coef(lm(df91$Penetracion~df91$PibMed))

g9 <- ggplot(df91,aes( x=PibMed, y=Penetracion  ,label = COMUNIDAD ))+ 
  geom_point(size = 2,color='red') +geom_text_repel() +
  theme(panel.background = element_rect(fill = '#F2F2F2', colour = 'black')) + 
  scale_color_manual(values=c("red", "pink", "blue","lime green", "black", "salmon","dark grey", "green", "yellow",
                              "orange", "green", "dark green","light blue", "dark green", "light green","purple", "dark grey"))+
  
  geom_abline(intercept = lr2[1], slope = lr2[2] ) + 
  theme(panel.grid.major =  element_line(), panel.grid.minor = element_line())+
  labs(title = "Penetración Total por Comunidad y PIB (2016)",x="Pib Per CÃ¡pita Relativo",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::percent_format())
g9
#######################################################################
# Grafico Penetracion Total Provincia y Comunidad
#######################################################################
mixto2016 <- mixto[mixto$variable=="2016",]
group10 <- group_by(mixto2016, PLAZA,COMUNIDAD,variable)

df10 <- summarise(group10,
                 Penetracion=sum(Penetracion, na.rm = TRUE),
                 PIB=sum(as.numeric(PIB), na.rm = TRUE)/length(PLAZA)
                 )
g10 <- ggplot(df10,aes( x=PIB, y=Penetracion  ,label = PLAZA ))+
  geom_point(size = 2,color='red') +geom_text_repel(colour="black", segment.colour="black", size=3,force=3) +
  labs(title = "Penetración Total por Comunidad y Provincia (2016)",x="Pib Per CÃ¡pita Relativo",y="Penetración") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1.5))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_continuous(labels = NULL)+
  scale_y_continuous(labels = scales::percent_format()) +
  facet_grid(. ~ COMUNIDAD)+facet_wrap(~ COMUNIDAD, nrow = 2,scales = "free_x")
g10
#######################################################################
# Grafico Penetracion Total vs HOLA PROVINCIA
#######################################################################
dfmerge_0 <- merge(x=df5 ,y=df5Hola,by.x=c("PLAZA","variable"),by.y=c("PLAZA","variable"))
dfmerge_1 <- as.data.frame(cbind(as.character(dfmerge_0$PLAZA),'TOTAL',as.character(dfmerge_0$variable),dfmerge_0$Penetracion.x))
dfmerge_2 <- as.data.frame(cbind(as.character(dfmerge_0$PLAZA),'HOLA' ,as.character(dfmerge_0$variable),dfmerge_0$Penetracion.y))

dfmerge <- rbind(dfmerge_1,dfmerge_2)
dfmerge$V4 <- as.numeric(as.character(dfmerge$V4))

g11 <- ggplot(dfmerge,aes( x=V3, y=V4,label = V2 ,group=V2)) +
  geom_point(col="black",size=2) +
  geom_line(aes(col=V2),size=1.5) +
  labs(title = "Penetración HOLA por Provincia y Año",x="",y="Penetración") +
  scale_color_manual(name = "Publicación",values=c("orange", "red")) +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ V1)+facet_wrap(~ V1, nrow = 3,scales = "free_x") +
  theme(strip.text.x = element_text(size =8, colour = "black", angle = 0))
g11
#######################################################################
# Grafico Penetracion SEMANA vs HOLA PROVINCIA
#######################################################################
dfmerge_0 <- merge(x=df5Semana ,y=df5Hola,by.x=c("PLAZA","variable"),by.y=c("PLAZA","variable"))
dfmerge_1 <- as.data.frame(cbind(as.character(dfmerge_0$PLAZA),'SEMANA',as.character(dfmerge_0$variable),dfmerge_0$Penetracion.x))
dfmerge_2 <- as.data.frame(cbind(as.character(dfmerge_0$PLAZA),'HOLA' ,as.character(dfmerge_0$variable),dfmerge_0$Penetracion.y))

dfmerge <- rbind(dfmerge_1,dfmerge_2)
dfmerge$V4 <- as.numeric(as.character(dfmerge$V4))

g111 <- ggplot(dfmerge,aes( x=V3, y=V4,label = V2 ,group=V2)) +
  geom_point(col="black",size=2) +
  geom_line(aes(col=V2),size=1.5) +
  labs(title = "Penetración HOLA vs SEMANA por Provincia y Año",x="",y="Penetración") +
  scale_color_manual(name = "Publicación",values=c("black", "red")) +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ V1)+facet_wrap(~ V1, nrow = 3,scales = "free_x") +
  theme(strip.text.x = element_text(size =8, colour = "black", angle = 0))
g111  
#######################################################################
# Grafico Penetracion LECTURAS vs HOLA PROVINCIA
######################################################################
dfmerge_0 <- merge(x=df5Lecturas ,y=df5Hola,by.x=c("PLAZA","variable"),by.y=c("PLAZA","variable"))
dfmerge_1 <- as.data.frame(cbind(as.character(dfmerge_0$PLAZA),'LECTURAS',as.character(dfmerge_0$variable),dfmerge_0$Penetracion.x))
dfmerge_2 <- as.data.frame(cbind(as.character(dfmerge_0$PLAZA),'HOLA' ,as.character(dfmerge_0$variable),dfmerge_0$Penetracion.y))

dfmerge <- rbind(dfmerge_1,dfmerge_2)
dfmerge$V4 <- as.numeric(as.character(dfmerge$V4))

g112 <- ggplot(dfmerge,aes( x=V3, y=V4,label = V2 ,group=V2)) +
  geom_point(col="black",size=2) +
  geom_line(aes(col=V2),size=1.5) +
  labs(title = "Penetración HOLA vs LECTURAS por Provincia y Año",x="",y="Penetración") +
  scale_color_manual(name = "Publicación",values=c("blue", "red")) +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ V1)+facet_wrap(~ V1, nrow = 3,scales = "free_x") +
  theme(strip.text.x = element_text(size =8, colour = "black", angle = 0))
g112
#######################################################################
# Grafico Penetracion DIEZ MINUTOS  vs HOLA PROVINCIA
#######################################################################
dfmerge_0 <- merge(x=df5DiezMinutos ,y=df5Hola,by.x=c("PLAZA","variable"),by.y=c("PLAZA","variable"))
dfmerge_1 <- as.data.frame(cbind(as.character(dfmerge_0$PLAZA),'DIEZ MINUTOS',as.character(dfmerge_0$variable),dfmerge_0$Penetracion.x))
dfmerge_2 <- as.data.frame(cbind(as.character(dfmerge_0$PLAZA),'HOLA' ,as.character(dfmerge_0$variable),dfmerge_0$Penetracion.y))

dfmerge <- rbind(dfmerge_1,dfmerge_2)
dfmerge$V4 <- as.numeric(as.character(dfmerge$V4))

g113 <- ggplot(dfmerge,aes( x=V3, y=V4,label = V2 ,group=V2)) +
  geom_point(col="black",size=2) +
  geom_line(aes(col=V2),size=1.5) +
  labs(title = "Penetración HOLA vs DIEZ MINUTOS por Provincia y Año",x="",y="Penetración") +
  scale_color_manual(name = "Publicación",values=c("dark green", "red")) +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ V1)+facet_wrap(~ V1, nrow = 3,scales = "free_x") +
  theme(strip.text.x = element_text(size =8, colour = "black", angle = 0))
g113
#######################################################################
# Grafico Penetracion Total Provincial vs HOLA COMUNIDAD
#######################################################################
dfmerge41_0 <- merge(x=df41 ,y=df41Hola,by.x=c("COMUNIDAD","variable"),by.y=c("COMUNIDAD","variable"))
dfmerge41_1 <- as.data.frame(cbind(as.character(dfmerge41_0$COMUNIDAD),'TOTAL',as.character(dfmerge41_0$variable),dfmerge41_0$Penetracion.x))
dfmerge41_2 <- as.data.frame(cbind(as.character(dfmerge41_0$COMUNIDAD),'HOLA' ,as.character(dfmerge41_0$variable),dfmerge41_0$Penetracion.y))

dfmerge41 <- rbind(dfmerge41_1,dfmerge41_2)
dfmerge41$V4 <- as.numeric(as.character(dfmerge41$V4))

g12 <- ggplot(dfmerge41,aes( x=V3, y=V4,label = V2 ,group=V2)) +
  geom_point(col="black",size=2) +
  geom_line(aes(col=V2),size=1.5) +
  labs(title = "Penetración HOLA vs Total por Comunidad y Año",x="",y="Penetración") +
  scale_color_manual(name = "Formato",values=c("orange", "red")) +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ V1)+facet_wrap(~ V1, nrow = 3,scales = "free_x") +
  theme(strip.text.x = element_text(size =8, colour = "black", angle = 0))
g12
#######################################################################
# Grafico Penetracion Semana vs HOLA COMUNIDAD
#######################################################################
dfmerge41_0 <- merge(x=df41Semana ,y=df41Hola,by.x=c("COMUNIDAD","variable"),by.y=c("COMUNIDAD","variable"))
dfmerge41_1 <- as.data.frame(cbind(as.character(dfmerge41_0$COMUNIDAD),'SEMANA',as.character(dfmerge41_0$variable),dfmerge41_0$Penetracion.x))
dfmerge41_2 <- as.data.frame(cbind(as.character(dfmerge41_0$COMUNIDAD),'HOLA' ,as.character(dfmerge41_0$variable),dfmerge41_0$Penetracion.y))

dfmerge41 <- rbind(dfmerge41_1,dfmerge41_2)
dfmerge41$V4 <- as.numeric(as.character(dfmerge41$V4))

g121 <- ggplot(dfmerge41,aes( x=V3, y=V4,label = V2 ,group=V2)) +
  geom_point(col="black",size=2) +
  geom_line(aes(col=V2),size=1.5) +
  labs(title = "Penetración HOLA vs SEMANA por Comunidad y Año",x="",y="Penetración") +
  scale_color_manual(name = "Formato",values=c("black", "red")) +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ V1)+facet_wrap(~ V1, nrow = 3,scales = "free_x") +
  theme(strip.text.x = element_text(size =8, colour = "black", angle = 0))
g121
#######################################################################
# Grafico Penetracion LECTURAS vs HOLA COMUNIDAD
#######################################################################
dfmerge41_0 <- merge(x=df41Lecturas ,y=df41Hola,by.x=c("COMUNIDAD","variable"),by.y=c("COMUNIDAD","variable"))
dfmerge41_1 <- as.data.frame(cbind(as.character(dfmerge41_0$COMUNIDAD),'LECTURAS',as.character(dfmerge41_0$variable),dfmerge41_0$Penetracion.x))
dfmerge41_2 <- as.data.frame(cbind(as.character(dfmerge41_0$COMUNIDAD),'HOLA' ,as.character(dfmerge41_0$variable),dfmerge41_0$Penetracion.y))

dfmerge41 <- rbind(dfmerge41_1,dfmerge41_2)
dfmerge41$V4 <- as.numeric(as.character(dfmerge41$V4))

g122 <- ggplot(dfmerge41,aes( x=V3, y=V4,label = V2 ,group=V2)) +
  geom_point(col="black",size=2) +
  geom_line(aes(col=V2),size=1.5) +
  labs(title = "Penetración HOLA vs LECTURAS por Comunidad y Año",x="",y="Penetración") +
  scale_color_manual(name = "Formato",values=c("blue", "red")) +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ V1)+facet_wrap(~ V1, nrow = 3,scales = "free_x") +
  theme(strip.text.x = element_text(size =8, colour = "black", angle = 0))
g122
#######################################################################
# Grafico Penetracion DIEZ MINUTOS vs HOLA COMUNIDAD
#######################################################################
dfmerge41_0 <- merge(x=df41DiezMinutos ,y=df41Hola,by.x=c("COMUNIDAD","variable"),by.y=c("COMUNIDAD","variable"))
dfmerge41_1 <- as.data.frame(cbind(as.character(dfmerge41_0$COMUNIDAD),'DIEZ MINUTOS',as.character(dfmerge41_0$variable),dfmerge41_0$Penetracion.x))
dfmerge41_2 <- as.data.frame(cbind(as.character(dfmerge41_0$COMUNIDAD),'HOLA' ,as.character(dfmerge41_0$variable),dfmerge41_0$Penetracion.y))

dfmerge41 <- rbind(dfmerge41_1,dfmerge41_2)
dfmerge41$V4 <- as.numeric(as.character(dfmerge41$V4))

g123 <- ggplot(dfmerge41,aes( x=V3, y=V4,label = V2 ,group=V2)) +
  geom_point(col="black",size=2) +
  geom_line(aes(col=V2),size=1.5) +
  labs(title = "Penetración HOLA vs DIEZ MINUTOS por Comunidad y Año",x="",y="Penetración") +
  scale_color_manual(name = "Publicación",values=c("dark green", "red")) +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(2013,2014,2015,2016)) +
  facet_grid(. ~ V1)+facet_wrap(~ V1, nrow = 3,scales = "free_x") +
  theme(strip.text.x = element_text(size =8, colour = "black", angle = 0))
g123
#######################################################################
# Graficos de Venta por Provincia
#######################################################################
mixtogroup13 <- group_by(mixto,Publicacion,variable,PLAZA)
df13 <- summarise(mixtogroup13,
                 Venta = sum(Venta, na.rm = TRUE))
df13 <- df13[order(df13$PLAZA),]

listaprov <- as.vector(unique(as.character(df13$PLAZA)))

#Bucle para crear los grÃ¡ficos
# i<- listaprov[11]

for (i in listaprov)
{
print(i)  
dff <- df13[as.character(df13$PLAZA)==i,]
provin <-i
maxgraf <- ceiling(max(dff$Venta)/2000)*2000

g13 <- ggplot(dff,aes(x=variable , y= Venta , group=Publicacion))+
  geom_line(aes(col=Publicacion),size=1)+
  geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) +
  scale_color_manual(name = "Publicación",values=c("dark green","red","blue","black"))+
  labs(title = paste("Venta Media (Formato Grande y Pequeño) ",provin),x="Año",y="Ejemplares Vendidos (OJD)") +
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_discrete( limits=c("2013","2014","2015","2016"), labels = scales::comma,breaks = seq(2013,2016,1))+
  scale_y_continuous(limits = c(0, maxgraf), labels = scales::comma,breaks = seq(0,maxgraf,2000))

ggsave(plot=g13,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", 
       filename=paste0("Venta Provincia ",i,".jpg"))
}
#######################################################################
# Graficos de Venta por Comunidad
#######################################################################
mixtogroup14 <- group_by(mixto,Publicacion,variable,COMUNIDAD)
df14 <- summarise(mixtogroup14,
                  Venta = sum(Venta, na.rm = TRUE))
df14 <- df14[order(df14$COMUNIDAD),]

listacom <- as.vector(unique(as.character(df14$COMUNIDAD)))

#Bucle para crear los grÃ¡ficos
# i <- listacom[1]
for (i in listacom)
{
  print(i)  
  dff <- df14[as.character(df14$COMUNIDAD)==i,]
  provin <-i
  maxgraf <- ceiling(max(dff$Venta)/5000)*5000
  
  g14 <- ggplot(dff,aes(x=variable , y= Venta , group=Publicacion))+
    geom_line(aes(col=Publicacion),size=1)+
    geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) +
    scale_color_manual(name = "Publicación",values=c("dark green","red","blue","black"))+
    labs(title = paste("Venta Media (Formato Grande y Pequeño) ",provin),x="Año",y="Ejemplares Vendidos (OJD)") +
    theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
    theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
    theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
    theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
    theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
    theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
    theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
    scale_x_discrete( limits=c("2013","2014","2015","2016"), labels = scales::comma,breaks = seq(2013,2016,1))+
    scale_y_continuous(limits = c(0, maxgraf), labels = scales::comma,breaks = seq(0,maxgraf,5000))
  
  ggsave(plot=g14,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", 
         filename=paste0("Venta Comunidad ",i,".jpg"))
}
#######################################################################
# Graficos de Venta por Comunidad y Formato
#######################################################################
mixtogroup15 <- group_by(mixto,Publicacion_Formato,variable,COMUNIDAD)
df15 <- summarise(mixtogroup15,
                  Venta = sum(Venta, na.rm = TRUE))
df15 <- df15[order(df15$COMUNIDAD),]

listacom <- as.vector(unique(as.character(df15$COMUNIDAD)))

# i <- listacom[1]

for (i in listacom)
{
  print(i)  
  dff <- df15[as.character(df15$COMUNIDAD)==i,]
  provin <-i
  maxgraf <- ceiling(max(dff$Venta)/5000)*5000
  
  g15 <- ggplot(dff,aes(x=variable , y= Venta , group=Publicacion_Formato))+
    geom_line(aes(col=Publicacion_Formato ,linetype=Publicacion_Formato),size=1.5 )+
    geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) + 
    scale_linetype_manual(name = "Publicación y formato",values=c("solid","dotted","solid","solid","dotted","solid","dotted")) +
    scale_color_manual(name = "Publicación y formato",values=c("dark green","dark green", "red","blue", "blue","black","black")) +
    labs(title = paste("Venta Media Publicación y Formato ",provin),x="Año",y="Ejemplares Vendidos (OJD)") +
    theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
    theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
    theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
    theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
    theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
    theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
    theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
    scale_x_discrete( limits=c("2013","2014","2015","2016"), labels = scales::comma,breaks = seq(2013,2016,1))+
    scale_y_continuous(limits = c(0, maxgraf), labels = scales::comma,breaks = seq(0,maxgraf,5000))
  ggsave(plot=g15,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", 
         filename=paste0("Venta Comunidad y Formato ",i,".jpg"))
}
#######################################################################
# Graficos de Venta por Provincia y Formato
#######################################################################

mixtogroup16 <- group_by(mixto,Publicacion_Formato,variable,PLAZA)
df16 <- summarise(mixtogroup16,
                  Venta = sum(Venta, na.rm = TRUE))
df16 <- df16[order(df16$PLAZA),]

listaPROV <- as.vector(unique(as.character(df16$PLAZA)))

# unique(as.character(mixto$Publicacion_Formato))
# unique(as.character(df15$Publicacion_Formato))

#Bucle para crear los grÃ¡ficos
# i <- listaPROV[1]

for (i in listaPROV)
{
  print(i)  
  dff <- df16[as.character(df16$PLAZA)==i,]
  provin <-i
  maxgraf <- ceiling(max(dff$Venta)/2000)*2000
  
  g16 <- ggplot(dff,aes(x=variable , y= Venta , group=Publicacion_Formato))+
    geom_line(aes(col=Publicacion_Formato ,linetype=Publicacion_Formato),size=1.5 )+
    geom_text_repel(aes(variable,Venta,label=format(Venta,big.mark=",")),force=1,colour="black", segment.colour="black", size=5.0) + 
    scale_linetype_manual(name = "Publicación y formato",values=c("solid","dotted","solid","solid","dotted","solid","dotted")) +
    scale_color_manual(name = "Publicación y formato",values=c("dark green","dark green", "red","blue", "blue","black","black")) +
    labs(title = paste("Venta Total Publicación y Formato ",provin),x="Año",y="Ejemplares Vendidos (OJD)") +
    theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
    theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
    theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
    theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
    theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
    theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
    theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
    scale_x_discrete( limits=c("2013","2014","2015","2016"), labels = scales::comma,breaks = seq(2013,2016,1))+
    scale_y_continuous(limits = c(0, maxgraf), labels = scales::comma,breaks = seq(0,maxgraf,2000))

  ggsave(plot=g16,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", 
         filename=paste0("Venta Provincia y Formato ",i,".jpg"))
}

#######################################################################
# Graficos de Penetración vs Concentración Comunidades
#######################################################################


mixtohola <- mixto[mixto$Publicacion=="HOLA",]
mixtogroup62 <- group_by(mixtohola, COMUNIDAD,variable)
df621 <- summarise(mixtogroup62,
                   pv = sum (PuntosVenta, na.rm = TRUE),
                   sup = sum (Superficie, na.rm = TRUE),
                   Penetracion = sum (Penetracion, na.rm = TRUE)/length(PLAZA),
                   ratio = sup/pv,
                   pvpob = sum(pvpob, na.rm = TRUE)/length(PLAZA))

# solo datos de 2016
df621 <- df621[df621$variable=="2016",]
# reordenar
df621 <- df621[ order(df62$pvpob), ]

df621$dens <- df621$pv/df621$sup

lr2 <- coef(lm(df621$Penetracion~df621$dens))

g621 <- ggplot(df621,aes( x=pv/sup, y=Penetracion  ,group=COMUNIDAD,label=COMUNIDAD))+geom_point(color='red')+
  #  geom_text(colour="black", size=4.0,hjust=0.5) +
  geom_text_repel()+
  labs(title = "Penetración HOLA vs Densidad de Puntos",x="Densidad de Puntos",y="Penetración") +
  theme(panel.background = element_rect(fill = '#F2F2F2', colour = 'black')) + 
  geom_abline(intercept = lr2[1], slope = lr2[2] ) + 
  theme(panel.grid.major =  element_line(), panel.grid.minor = element_line())+
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::percent_format())
g621

#######################################################################
# Graficos de Penetración vs Concentración Provincias
#######################################################################
mixtohola <- mixto[mixto$Publicacion=="HOLA",]
mixtogroup622 <- group_by(mixtohola, PLAZA,variable)
df622 <- summarise(mixtogroup622,
                   pv = sum (PuntosVenta, na.rm = TRUE),
                   sup = sum (Superficie, na.rm = TRUE),
                   Penetracion = sum (Penetracion, na.rm = TRUE)/length(PLAZA),
                   ratio = sup/pv,
                   pvpob = sum(pvpob, na.rm = TRUE)/length(PLAZA))

# solo datos de 2016
df622 <- df622[df622$variable=="2016",]

df622$dens <- df622$pv/df622$sup

lr22 <- coef(lm(df622$Penetracion~df622$dens))

g622 <- ggplot(df622,aes( x=pv/sup, y=Penetracion  ,group=PLAZA,label=PLAZA))+geom_point(color='red')+
  #  geom_text(colour="black", size=4.0,hjust=0.5) +
  geom_text_repel()+
  labs(title = "Penetración HOLA vs Densidad de Puntos",x="Densidad de Puntos",y="Penetración") +
  theme(panel.background = element_rect(fill = '#F2F2F2', colour = 'black')) + 
  geom_abline(intercept = lr2[1], slope = lr2[2] ) + 
  theme(panel.grid.major =  element_line(), panel.grid.minor = element_line())+
  theme(plot.title   = element_text(family="Albany AMT",colour="black",hjust = 0.5,face="bold", size=rel(2))) +
  theme(legend.title = element_text(hjust = 0,face="plain", size=rel(1.2))) +
  theme(legend.text  = element_text(hjust = 0,face="plain", size=rel(1))) +
  theme(axis.title.x = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0,hjust=0,face="plain", colour="black", size=rel(1.2))) +
  theme(axis.text.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="plain", vjust=-0.5, colour="black", size=rel(1.5))) + 
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::percent_format())
g622


ggsave(plot=g1            ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='Venta Total.jpg')
ggsave(plot=g2            ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='Venta Total Formato.jpg')
ggsave(plot=g21           ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='Venta Sem Formato.jpg')
ggsave(plot=g22           ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='Venta Lec Formato.jpg')
ggsave(plot=g23           ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='Venta DM Formato.jpg')
ggsave(plot=g24           ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='Venta Hola Formato.jpg')
ggsave(plot=g3            ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='Pen Total Prov.jpg')
ggsave(plot=g41           ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='Pen Total Prov Año.jpg')
ggsave(plot=g42           ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='Pen Total Com Año.jpg')
ggsave(plot=g41Hola       ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenetHola.jpg')
ggsave(plot=g41DiezMinutos,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenetDiezMinutos.jpg')
ggsave(plot=g41Semana     ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenetSemana.jpg')
ggsave(plot=g41Lecturas   ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenetLecturas.jpg')
ggsave(plot=g5            ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenTotProv.jpg')
ggsave(plot=g5Hola        ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenTotProvHola.jpg')
ggsave(plot=g5DiezMinutos ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenTotProvDiezMinutos.jpg')
ggsave(plot=g5Semana      ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenTotProvSemana.jpg')
ggsave(plot=g5Lecturas    ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenTotProvLecturas.jpg')
ggsave(plot=g6            ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PubObjetivoPVProv.jpg')
ggsave(plot=g62           ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PubObjetivoPVCom.jpg')
ggsave(plot=g7            ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PunVentSupProv.jpg')
ggsave(plot=g72           ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PunVentSupCom.jpg')
ggsave(plot=g8            ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PentTotProvPIB.jpg')
ggsave(plot=g9            ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PentTotComPIB.jpg')
ggsave(plot=g10           ,device="png", width = 16, height = 8,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenTotProvComPIB.jpg')
ggsave(plot=g11           ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenTotHolaProv.jpg')
ggsave(plot=g111          ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenSemHolaProv.jpg')
ggsave(plot=g112          ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenLecHolaProv.jpg')
ggsave(plot=g113          ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenDMHolaProv.jpg')
ggsave(plot=g12           ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenTotHolaCom.jpg')
ggsave(plot=g121          ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenSemHolaCom.jpg')
ggsave(plot=g122          ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenLecHolaCom.jpg')
ggsave(plot=g123          ,device="png", width = 18, height = 9,dpi=200,path = "C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/", filename='PenDMHolaCom.jpg')



summary(mixto)

mixtohola <- mixto[mixto$Publicacion=="HOLA",]
mixtogroupcuota <- group_by(mixto, PLAZA,variable)
dfcuota <- summarise(mixtogroupcuota,
                   Venta = sum (Venta, na.rm = TRUE))

dfcuota
head(mixto,10)

mixtocuota <- merge(x=mixto,y=dfcuota,by.x = c("PLAZA","variable"),by.y =c ("PLAZA","variable"))
head(mixtocuota,7)

mixtocuota$cuota <- mixtocuota$Venta.x/mixtocuota$Venta.y



datatable(mixtocuota, options = list(pageLength = 25,"autoWidth"= FALSE))
