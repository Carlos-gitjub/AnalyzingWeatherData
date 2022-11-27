library(reshape2)
library(tidyverse)
library(moments)
library(GGally)
# library(tidyquant)
# library(e1071)

estaciones <- read.delim("estaciones_meteo.txt",sep="\t")
estaciones <- estaciones %>% filter(estacion %in% c("MADRID","BARCELONA","SEVILLA","ZARAGOZA","BILBAO","VIGO"))
meteo_data <- read.delim("meteo_data.csv",sep=";",stringsAsFactors = FALSE)
meteo_data <- meteo_data %>% mutate(Date=as.Date(Date))
meteo_data <- meteo_data %>% filter(estacion %in% estaciones$estacion)

summary(meteo_data)
# histograma para la estación de Zaragoza de la temperatura media
est="ZARAGOZA"
ggplot(meteo_data %>% filter(estacion==est))+ geom_histogram(aes(Mean_TemperatureC), binwidth = 1) 

# histogramas para todas las estaciones de la temperatura media
var="Mean_TemperatureC"
ggplot(meteo_data)+ geom_histogram(aes_string(var, color="estacion"), position = "identity", fill=NA, binwidth=2.5)+ 
  scale_color_brewer(palette = "Set1")

# polígonos de frecuencia para ver mejor las diferencias
meteo_data %>% 
  ggplot() + geom_freqpoly(aes(Mean_TemperatureC, color = estacion), position = "identity") +
  scale_color_brewer(palette = "Set1")

# densidads de frecuencia ajustadas
meteo_data %>% ggplot() + geom_density(aes(Mean_TemperatureC, color = estacion)) +
  scale_color_brewer(palette = "Set1")

# densidades de variable Precipitación
meteo_data %>% 
  ggplot() + geom_histogram(aes(Precipitationmm,color=estacion),position = "identity",fill=NA,binwidth = 2.5) +
  scale_color_brewer(palette = "Set1")

# densidad usando logaritmo (ya que distribuciones muy concentradas en el 0)
meteo_data %>% mutate(Precipitationmm = Precipitationmm+0.01) %>% #Para poder ver el 0 en esa escala
  ggplot() + geom_density(aes(Precipitationmm,color=estacion)) +
  scale_color_brewer(palette = "Set1") + scale_x_log10()

# Transformamos datos a formato long
meteo_long <- melt(meteo_data, id=c("estacion", "Date"), 
                   measure.vars = c("Mean_TemperatureC","Mean_Wind_SpeedKm_h","WindDirDegrees", "Precipitationmm","CloudCover","Mean_Humidity"))

# Distribuciones densidad de variables: Mean_TemperatureC, Mean_Wind_SpeedKm_h, WindDirDegrees, Precipitationmm, CloudCover, Mean_Humidity
meteo_long %>% 
  ggplot() + geom_density(aes(value, color=estacion)) + facet_wrap(~variable, scales = "free") + 
  scale_color_brewer(palette = "Set1")

# Boxplot Temperatura máxima (Max_TemperatureC)
ggplot(meteo_data) + geom_boxplot(aes(estacion, Max_TemperatureC))

# facets de los boxplots de las diferentes variables
meteo_long %>% 
  ggplot() + geom_boxplot(aes(estacion, value,color=estacion)) + facet_wrap(~variable,scales = "free") +
  scale_color_brewer(palette = "Set1") + theme(legend.position = 'none',axis.text.x = element_text(angle=90))

# Variables categóricas
# total de dias que ha llovido por estación
( df_lluvia <- meteo_data %>% select(estacion,Rain) %>% group_by(estacion) %>% filter(Rain=="TRUE") %>% 
    count() %>% arrange(desc(n)) %>% as.data.frame() )

table(meteo_data$estacion,meteo_data$Rain)
# estación con mas eventos de lluvia
( df_lluvia %>% select(estacion,n) %>% filter(n==max(n)) )

# numero de dias con precipitacion mayor que 0 por estacion
( df_precipitacion <- meteo_data %>% select(estacion, Precipitationmm) %>% group_by(estacion) %>% filter(Precipitationmm > 0) %>% count() %>% arrange(desc(n)) %>% as.data.frame() )

table(meteo_data$estacion, meteo_data$Precipitationmm > 0)

# de niebla
( df_niebla <- meteo_data %>% select(estacion, Fog) %>% group_by(estacion) %>% filter(Fog == TRUE) %>% count() %>% arrange(desc(n)) )
table(meteo_data$estacion, meteo_data$Fog)

# Gráficos con el conteo de todas las variables
meteo_cat <- melt(meteo_data,id=c("estacion","Date"),
                  measure.vars = c("Fog","Hail","Rain","Snow","Thunderstorm","Tornado"))

meteo_cat %>% 
  ggplot() + geom_bar(aes(value,fill=estacion),position="dodge") + facet_wrap(~variable,scales = "free") +
  scale_fill_brewer(palette = "Set1") 

meteo_cat %>% 
  ggplot() + geom_bar(aes(value,fill=value)) + facet_grid(variable~estacion,scales = "free") +
  theme(legend.position = 'none')

# Medidas de posición Mean_Wind_SpeedKm_h
# Media Mean_Wind_SpeedKm_h
meteo_long %>% filter(variable=="Mean_Wind_SpeedKm_h") %>% group_by(estacion) %>% summarise(meanWindSpeed=mean(value)) %>% arrange(desc(meanWindSpeed))
# Mediana Mean_Wind_SpeedKm_h
meteo_long %>% filter(variable=="Mean_Wind_SpeedKm_h") %>% group_by(estacion) %>% summarise(medianWindSpeed=median(value)) %>% arrange(desc(medianWindSpeed))
# Media truncada Mean_Wind_SpeedKm_h
meteo_long %>% filter(variable=="Mean_Wind_SpeedKm_h") %>% group_by(estacion) %>% 
  summarise(trimmedMeanWindSpeed=mean(value, trim = 0.1))

# Para variables Mean_TemperatureC, Mean_Wind_SpeedKm_h, WindDirDegrees, Precipitationmm ,CloudCover ,Mean_Humidity
medias<-meteo_long %>% group_by(estacion,variable) %>% summarise(mean=mean(value,na.rm=TRUE),median=median(value,na.rm=TRUE),trimmean=mean(value,trim=0.1,na.rm=TRUE))
medias
# Sin transformar los datos a tipo long
mediasw <- meteo_data %>%  group_by(estacion) %>% 
  summarise_each(funs(mean=mean(.,na.rm=TRUE),median=median(.,na.rm=TRUE),trimmean=mean(.,trim=0.1,na.rm=TRUE)),Max_TemperatureC:Mean_Humidity)
mediasw
# Seleccion de solo las medias normales de las variables de temperatura
mediasw %>% select(estacion,matches("Temper(.*)_mean"))
# Representando los resultados de forma gráfica
( melt(medias, id=c("estacion","variable"), variable.name = "medida") )

ggplot(melt(medias,id=c("estacion","variable"),variable.name = "medida")) +
  geom_point(aes(estacion, value, color=medida)) + 
  geom_line(aes(estacion,value, color=medida, group=medida)) +
  facet_wrap(~variable,scales = "free") + theme(axis.text.x = element_text(angle=90))


# Medidas de dispersión (Precipitacionmm)

# Desviación típica e Intervalo intercuartílico
meteo_data %>% group_by(estacion) %>% summarise(desviacionTipicaPrecipitacionmm=sd(Precipitationmm),IQR_Precipitationmm=IQR(Precipitationmm))
# variables Mean_TemperatureC, Mean_Wind_SpeedKm_h,WindDirDegrees, Precipitationmm ,CloudCover ,Mean_Humidity
meteo_sd_iqr<-melt(meteo_data, id=c("estacion","Date"), measure.vars = c("Mean_TemperatureC","Mean_Wind_SpeedKm_h","WindDirDegrees","Precipitationmm","CloudCover","Mean_Humidity")) %>% 
  as.data.frame() %>%
  group_by(estacion,variable) %>%
  summarise(sd=sd(value, na.rm = TRUE),IQR=IQR(value,na.rm = TRUE))

meteo_sd_iqr
# Grafica de los resultados
ggplot(melt(meteo_sd_iqr, id=c("estacion","variable"),variable.name = "medida")) +
  geom_bar(aes(estacion,value,fill=medida), stat = "identity", position = "dodge") +
  facet_wrap(~variable, scales = "free") + 
  theme(axis.text.x=element_text(angle=90))

ggplot(melt(meteo_sd_iqr, id=c("estacion","variable"),variable.name = "medida")) +
  geom_point(aes(estacion,value,color=medida)) +
  geom_line(aes(estacion,value,color=medida,group=medida)) +
  facet_wrap(~variable,scales = "free") +
  theme(axis.text.x = element_text(angle = 90))



## Cuantiles y boxplots personalizados
# Transformación de los datos.
# Para las variables de tipo numérico de antes, dataframe con los cuantiles 0.05,0.25,0.5,0.75 y 0.95.
# también la media y la desviación típica
vnames<-as.character(unique(meteo_long$variable))
enames<-as.character(unique(meteo_long$estacion))
df<-data.frame(estacion=character(),variable=character(),sd=numeric(),mean=numeric(),q0=numeric(),q5=numeric(),q25=numeric(),q50=numeric(),q75=numeric(),q95=numeric(),q100=numeric())
for (i in 1:n_distinct(meteo_long$estacion)) {
  for (j in 1:n_distinct(meteo_long$variable)) {
    x_<-meteo_long %>% filter(estacion==enames[i],variable==vnames[j]) %>% select(value)
    sd<-sd(x_$value,na.rm=TRUE)
    mean<-mean(x_$value,na.rm=TRUE)
    q<-quantile(x_, c(0,0.05,0.25,0.5,0.75,0.95,1),na.rm=TRUE)    
    r<-nrow(df)+1
    df[r,"estacion"]<-enames[i]
    df[r,"variable"]<-vnames[j]
    df[r,"sd"]<-sd
    df[r,"mean"]<-mean
    df[r,"q0"]<-q[1]
    df[r,"q5"]<-q[2]
    df[r,"q25"]<-q[3]
    df[r,"q50"]<-q[4]
    df[r,"q75"]<-q[5]
    df[r,"q95"]<-q[6]
    df[r,"q100"]<-q[7]
  }
}
df

# Grafica de los datos
df %>%
  ggplot() +
  geom_boxplot(aes(x=estacion,ymin=q5,lower=q25,middle=q50,upper=q75,ymax=q95),stat = "identity") +
  geom_pointrange(aes(estacion,y=mean,ymin=mean-sd,ymax=mean+sd),color="red") +
  facet_wrap(~variable,scales = "free") +
  theme(axis.text.x = element_text(angle = 45))


## Forma de las distribuciones
# Coeficientes de variación, Skewness y Kurtosis
m_forma<-meteo_long %>% group_by(estacion,variable) %>% 
  summarise(mean=mean(value,na.rm=TRUE),
            sd=sd(value,na.rm=TRUE),
            coef_var=(sd(value,na.rm=TRUE)/abs(mean(value,na.rm=TRUE))),
            sk=skewness(value,na.rm=TRUE),
            k=kurtosis(value,na.rm=TRUE))

m_forma
# CV
ggplot(m_forma) +
  geom_bar(aes(estacion,coef_var,fill=estacion), stat = "identity") +
  facet_wrap(~variable, scales = "free_y") +
  theme(legend.position = "none",axis.text.x = element_text(angle=45))

# grafica todas las medidas para todas las estaciones
ggplot(melt(m_forma,id=c("estacion","variable"),variable.name = "medida")) + 
  geom_bar(aes(estacion, value,fill=variable), stat = "identity") +
  # geom_text(aes(estacion,0.8*value, label=round(value,2)),size=2.5) +
  facet_wrap(~paste(variable,"-",medida),scales = "free_y",ncol = 5) +
  theme(legend.position="none",axis.text.x = element_text(angle=90),strip.text = element_text(color='blue',size=6))


# correlacion estación Barcelona (tipo numericas)
vars<-meteo_data %>% filter(estacion=="BARCELONA")%>% select_if(is.numeric)

# matriz de correlación temperatura y nubosidad
meteo_data %>% filter(estacion=="BARCELONA") %>% 
  ggpairs(columns = 4:12)

# Matriz de correlacion para todas las estaciones a la vez
ggpairs(meteo_data, mapping = aes(color = estacion), columns = 4:12)

# matriz de correlación para solo los valores numéricos
corw <- meteo_data %>%  group_by(estacion) %>% do(corm={
  df<- as.data.frame(cor(.[4:12],use="complete"))
  cbind(variable1=row.names(df),df)
}) %>% ungroup() %>%  unnest()

corw



