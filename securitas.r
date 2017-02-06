# install_github('ramnathv/slidify@fix_encode')
library(RODBC)
library(plyr)
library(dygraphs)
library(reshape2)
library(xts)
library(TTR)
library(htmlwidgets)

# Abrienndo conexión con BD via FTP 
channel <- odbcConnect("secdir32","secdir","sec!73.");

# Query the database and put the results into the data frame "dataframe"
dataframe <- sqlQuery(channel, "select case when Pri.co_prioridad||'Tot' = 'IntTot' and (extract(hour from hh_salto)  between 0 and 6 or extract(hour from hh_salto)  = 23) then 'IntTot.Noche' 
            when Pri.co_prioridad||'Tot' = 'IntTot' and (extract(hour from hh_salto) between 7 and 14 ) then 'IntTot.Mannana'
                      when Pri.co_prioridad||'Tot' = 'IntTot' and (extract(hour from hh_salto)  between 15 and 22 ) then 'IntTot.Tarde' 
                      
                      when Pri.co_prioridad||'Tot' = 'EmeTot' and (extract(hour from hh_salto)  between 0 and 6 or extract(hour from hh_salto)  = 23) then 'EmeTot.Noche' 
                      when Pri.co_prioridad||'Tot' = 'EmeTot' and (extract(hour from hh_salto) between 7 and 14 ) then 'EmeTot.Mannana'
                      when Pri.co_prioridad||'Tot' = 'EmeTot' and (extract(hour from hh_salto)  between 15 and 22 ) then 'EmeTot.Tarde' 
                      
                      when Pri.co_prioridad||'Tot' = 'InfTot' and (extract(hour from hh_salto)  between 0 and 6 or extract(hour from hh_salto)  = 23) then 'InfTot.Noche' 
                      when Pri.co_prioridad||'Tot' = 'InfTot' and (extract(hour from hh_salto) between 7 and 14 ) then 'InfTot.Mannana'
                      when Pri.co_prioridad||'Tot' = 'InfTot' and (extract(hour from hh_salto)  between 15 and 22 ) then 'InfTot.Tarde' 
                      
                      else Pri.co_prioridad||'Tot'  end as tipoAlarma,
                      
                      dt_salto as valueDate,                              
                      sum(qt_saltos) as val                                          
                      from                                                                  
                      SAl_F_Saltos_Alarma_A01        SAl,                                 
                      SAl_D_Prioridad_Grupo          PGr,                                 
                      SAl_D_Prioridad                Pri,                                 
                      SAl_D_Prioridad_Tipo_Hie       Hie,                                 
                      SAl_D_Tipo_Alarma              Tip                                  
                      where SAl.id_prioridad               = PGr.id_prioridad               
                      and SAl.id_tipo_alarma             = Hie.id_tipo_alarma             
                      and PGr.id_prioridad_grupo         = Hie.id_prioridad_grupo         
                      and PGr.id_prioridad               = Pri.id_prioridad               
                      and Hie.id_tipo_alarma             = Tip.id_tipo_alarma             
                      and to_char(dt_salto, 'yyyymmdd') >= '20160101'          and  to_char(dt_salto, 'yyyymmdd') not in ( '20150321', '20150322')                         
                      group by 1 ,2                                                           
                      union                                                                   
                      select case when PGr.co_prioridad_grupo = 'InfRes' and (extract(hour from hh_salto)  between 0 and 6 or extract(hour from hh_salto)  = 23) then 'InfRes.Noche' 
                      when PGr.co_prioridad_grupo = 'InfRes' and (extract(hour from hh_salto) between 7 and 14 ) then 'InfRes.Mannana'
                      when PGr.co_prioridad_grupo = 'InfRes' and (extract(hour from hh_salto)  between 15 and 22 ) then 'InfRes.Tarde' 
                      else PGr.co_prioridad_grupo  end as tipoAlarma,                        
                      dt_salto as valueDate, 
                      sum(qt_saltos) as val                                          
                      from                                                                  
                      SAl_F_Saltos_Alarma_A01        SAl,                                 
                      SAl_D_Prioridad_Grupo          PGr,                                 
                      SAl_D_Prioridad                Pri,                                 
                      SAl_D_Prioridad_Tipo_Hie       Hie,                                 
                      SAl_D_Tipo_Alarma              Tip                                  
                      where SAl.id_prioridad               = PGr.id_prioridad               
                      and SAl.id_tipo_alarma             = Hie.id_tipo_alarma             
                      and PGr.id_prioridad_grupo         = Hie.id_prioridad_grupo         
                      and PGr.id_prioridad               = Pri.id_prioridad               
                      and Hie.id_tipo_alarma             = Tip.id_tipo_alarma             
                      and to_char(dt_salto, 'yyyymmdd') >= '20160101'             and  to_char(dt_salto, 'yyyymmdd') not in ( '20150321', '20150322')                   
                      group by 1, 2    
                      union
                      select  PGr.co_prioridad_grupo as tipoAlarma,                        
                      dt_salto as valueDate, 
                      sum(qt_saltos) as val                                          
                      from                                                                  
                      SAl_F_Saltos_Alarma_A01        SAl,                                 
                      SAl_D_Prioridad_Grupo          PGr,                                 
                      SAl_D_Prioridad                Pri,                                 
                      SAl_D_Prioridad_Tipo_Hie       Hie,                                 
                      SAl_D_Tipo_Alarma              Tip                                  
                      where SAl.id_prioridad               = PGr.id_prioridad               
                      and SAl.id_tipo_alarma             = Hie.id_tipo_alarma             
                      and PGr.id_prioridad_grupo         = Hie.id_prioridad_grupo         
                      and PGr.id_prioridad               = Pri.id_prioridad               
                      and Hie.id_tipo_alarma             = Tip.id_tipo_alarma   
                      and  PGr.co_prioridad_grupo        = 'InfRes'          
                      and to_char(dt_salto, 'yyyymmdd') >= '20160101'             and  to_char(dt_salto, 'yyyymmdd') not in ( '20150321', '20150322')                   
                      group by 1, 2                                                           
                      order by tipoalarma,valueDate")


forecast <- sqlQuery(channel, "select output||'.For' as output,valuedate,round(forecast) as prevision
                                from forecastseries
                                where 
                                forsession='E.20161215.02.Forecast.Combinada'
                                and output not in ('EmeTot','InfTot','IntTot')
                                order by output,valueDate")

head(dataframe,10)
securitas  <- dataframe
head(securitas)
head(forecast)

str(securitas)

unique(securitas$tipoalarma)
unique(dataframe$tipoalarma)

securitas2 <- dcast(data = securitas,formula = valuedate~tipoalarma ,fun.aggregate = sum) 
forecast2  <- dcast(data = forecast,formula = valuedate~output ,fun.aggregate = sum) 


head(securitas2)
head(forecast2)


summary(securitas2)


securitas2 <- as.xts(securitas2[,-1],order.by=as.POSIXct(securitas2$valuedate))
forecast2   <- as.xts(forecast2[,-1],order.by=as.POSIXct(forecast2$valuedate))
mixtoxts <- merge(securitas2,forecast2,all=TRUE)

colnames(mixtoxts)[1]


#dygraph(mixtoxts,main="secs")

for (i in 1:22)
{ print(i)
  pl <-  dygraph(mixtoxts[,c(i,i+22)],main=paste0(colnames(mixtoxts)[i],'   -   ',colnames(mixtoxts)[i+22]))
  saveWidget(pl, file=paste0("C:/dy",i,'.html'), selfcontained = TRUE,background = "white")
}

slidify("C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/markdown/a2/index.Rmd")
browseURL("C:/Users/fjarroyo/Documents/_DOCUMENTOS BAYES/R/markdown/a2/index.html")













dygraph(mixtoxts[,c(1,23)],main="secs")


dygraph(merge(mixtoxts$EmeRes,mixtoxts$EmeRes.1),main="secs")

dygraph(merge(mixtoxts$InfCoC,mixtoxts$InfCoC.1),main="secs")


dygraph(merge(mixtoxts[,$EmeRes,mixtoxts$EmeRes.1),main="secs")


dygraph(mixtoxts,main="secs")


dygraph(securitas2$[,c(5,9,10,11)],main="secs") %>%
  dySeries("InfCoC", drawPoints = TRUE, color = "blue") 


mav <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}
mav(securitas2[,5],10)

securitas2$InfCoCMA <- SMA(securitas2[,5],10)

dygraph(securitas2[,5],main="secs")+
  dygraph(SMA(securitas2[,5],10),main="secs")
  

dygraph(forecast2[,5],main="forecast")

  
  dygraph(securitas2[,c(5,13)],main="secs") %>%
  dySeries("InfCoC", drawPoints = F, color = "blue")  %>%
  dySeries("InfCoCMA", drawPoints = FALSE, color = "red") 

  
dygraph(securitas2,main="secs")
  


securitas2[,13]

dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dySeries("mdeaths", drawPoints = TRUE, color = "blue") %>%
  dySeries("fdeaths", stepPlot = TRUE, color = "red")   


str(lungDeaths)
class(lungDeaths)
dygraph(lungDeaths)
