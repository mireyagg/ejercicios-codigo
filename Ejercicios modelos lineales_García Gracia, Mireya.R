getwd
setwd("directorio de trabajo")
library(foreign)
library(dplyr)
library(ggplot2)
library(modeest)
library(moments)
library(BSDA) # para intervalos de confianza
library(nortest)
library(car)
library(gmodels)
library(vcd)

datos<- read.table("datos.csv", header=T, sep=";", dec=".")


datos$ESPECIEcod <- factor(datos$ESPECIE)
#labels=c("Litopenaeus vannamei","Penaeus japonicus","Penaeus monodon","Penaeus stylirostris")) # labels=c("Litopenaeus vannamei","Penaeus japonicus","Penaeus monodon","Penaeus stylirostris");
datos$PRESENTcod <- factor(datos$PRESENT) # labels=c("Cola","Entero"))
datos$PR_COLAcod <- factor(datos$PR_COLA) # labels=c("Pelado","Sin pelar"))
datos$TRATcod <- factor(datos$TRAT) # labels=c("Cocido","Crudo"))
datos$ASPECTOcod <- factor(datos$ASPECTO) # labels=c("Bueno", "Malo", "Regular"))
datos$PAIScod <- factor(datos$PAIS) # labels=c("Belize","Ecuador","Espa?a", "Nicaragua","Tailandia","Vietnam"))
datos$SISTEMAcod <- factor(datos$SISTEMA) # ,labels=c("Extensivo","Intensivo","Semi-intensivo"))
datos$CONGELACIONcod <-factor(datos$CONGELACION) # labels=c("-18?","-40?"))
datos$OTCcod <- factor(datos$OTC)
datos$SScod <- factor(datos$SS)
datos$LMcod <- factor(datos$LM)


#DEBEMOS COMPROBAR LA NORMALIDAD DE LAS VARIABLES PARA DETERMINAR QUÉ COEFICIENTE DE CORRELACIÓN LINEAL DEBEMOS CALCULAR
ks.test(x = datos$PESO_FRESCO,"pnorm") # NO normal
ks.test(x = datos$PESO_CONG,"pnorm") # NO normal
ks.test(x = datos$PESO_DESCONG,"pnorm") # NO normal

#COMO NINGUNA DE LAS VARIABLES DE PESO SIGUE UNA DISTRIBUCIÓN NORMAL, EL COEFICIENTE DE CORRELACIÓN QUE UTILIZAREMOS ES SPEARMAN, NO PEARSON

#EJERCICIO 1: Realizar el análisis de correlación y regresión entre el peso descongelado y congelado

#Coeficiente de correlación:
cor.test(x = datos$PESO_CONG,
         y = datos$PESO_DESCONG, 
         alternative = "two.sided",
         conf.level  = 0.95,
         method      = "spearman")
#r = 0.9704, muy cercano a 1, p-valor=0.001***, se puede afirmar que la correlación es significativa, el peso descongelado depende del peso congelado
#r^2 = 0,9417, el 94,17% de la variable PESO_DESCONGELADO se explica con un modelo de regresión basado en la variable peso congelado

#Regresión con análisis de residuos:
regresion <- lm(datos$PESO_DESCONG ~ datos$PESO_CONG, data = datos) 
summary(regresion)
#OBTENEMOS UNA PENDIENTE DE 0.9727 (P-VALOR = 0.001***), POR LO QUE PODEMOS AFIRMAR QUE LA PENDIENTE SÍ ES SIGNIFICATIVA, POR LO QUE EL MODELO DE REGRESIÓN TAMBIÉN LO ES 
#EN ESTE CASO LA ORDENADA EN EL ORIGEN TAMBIÉN ES SIGNIFICATIVA
datos$PESO_DESCONG_ESTIMADO <-  -2.94 + (0.97271*datos$PESO_CONG)

#Gráficos de residuos 
plot(datos$PESO_CONG, datos$PESO_DESCONG, xlab='peso congelado', ylab='peso descongelado',col="lightblue")
abline(regresion)
#PARECE EXISTIR UNA CORRELACIÓN SIGNIFICATIVA ENTRE EL PESO DESCONGELADO Y EL CONGELADO: EL PESO DESCONGELADO DEPENDERÍA DEL CONGELADO
#SE DEBE REALIZAR UN ANÁLISIS DE RESIDUOS QUE VALIDE EL MODELO:

#ANÁLISIS DE RESIDUOS:
plot(datos$PESO_DESCONG_ESTIMADO-datos$PESO_DESCONG) #la varianza sí que es constante, 
mean(datos$PESO_DESCONG_ESTIMADO-datos$PESO_DESCONG) #la media está cerca de 0
ks.test(datos$PESO_DESCONG_ESTIMADO-datos$PESO_DESCONG,"pnorm") #NO sigue una distribución normal
qqnorm(datos$PESO_DESCONG_ESTIMADO-datos$PESO_DESCONG)
qqline(datos$PESO_DESCONG_ESTIMADO-datos$PESO_DESCONG)
#EL ANÁLISIS DE RESIDUOS INDICA QUE NO SE CUMPLEN LAS CONDICIONES DE LA HIPÓTESIS, POR LO QUE EL MODELO NO ES VÁLIDO
#AUNQUE EL GRÁFICO DE REGRESIÓN PARECE INDICAR QUE EXISTE UNA CORRELACIÓN, EL ANÁLISIS DE RESIDUOS INDICA QUE EL MODELO NO ES VÁLIDO
#NO PODRÍAMOS CONFIRMAR LA CORRELACIÓN ENTRE EL PESO CONGELADO Y DESCONGELADO CON ESTE MODELO

#EJERCICIO 2: Realizar el análisis de correlación y regresión entre el peso descongelado y fresco

#Coeficiente de correlación
cor.test(x = datos$PESO_FRESCO,
         y = datos$PESO_DESCONG, 
         alternative = "two.sided",
         conf.level  = 0.95,
         method      = "spearman")
#r = 0.9877, muy cercano a 1, p-valor=0.001***, se puede afirmar que la correlación es significativa, el peso descongelado depende del peso fresco
#r^2 = 0,9755 el 97,55% de la variable PESO_DESCONGELADO se explica con un modelo de regresión basado en el peso fresco

#Regresión con análisis de residuos:
regresion <- lm(datos$PESO_DESCONG ~ datos$PESO_FRESCO, data = datos) 
summary(regresion)
#OBTENEMOS UNA PENDIENTE DE 0.9981 (P-VALOR = 0.001***), POR LO QUE PODEMOS AFIRMAR QUE LA PENDIENTE SÍ ES SIGNIFICATIVA, POR LO QUE EL MODELO DE REGRESIÓN TAMBIÉN LO ES
#ADEMÁS, EN ESTE CASO LA ORDENADA EN EL ORIGEN TAMBIÉN ES SIGNIFICATIVA: -0.6647 (P-VALOR=0.0005***)
datos$PESO_DESCONG_ESTIMADO1 <- -0.6647 + 0.9981*datos$PESO_FRESCO

#Gráficos de residuos 
plot(datos$PESO_FRESCO, datos$PESO_DESCONG, xlab='peso fresco', ylab='peso descongelado',col="pink")
abline(regresion)
#PARECE EXISTIR UNA CORRELACIÓN SIGNIFICATIVA ENTRE EL PESO DESCONGELADO Y EL FRESCO: EL PESO DESCONGELADO DEPENDE DEL FRESCO

#ANÁLISIS DE RESIDUOS:
plot(datos$PESO_DESCONG_ESTIMADO1-datos$PESO_DESCONG) #la varianza sí que es constante, 
mean(datos$PESO_DESCONG_ESTIMADO1-datos$PESO_DESCONG) #la media está cerca de 0
ks.test(datos$PESO_DESCONG_ESTIMADO1-datos$PESO_DESCONG,"pnorm") #sí es normal
qqnorm(datos$PESO_DESCONG_ESTIMADO1-datos$PESO_DESCONG)
qqline(datos$PESO_DESCONG_ESTIMADO1-datos$PESO_DESCONG) #se desvía por los extremos
#EL ANÁLISIS DE RESIDUOS INDICA QUE SE CUMPLEN LAS CONDICIONES DE LA HIPÓTESIS, LO CUAL VALIDA EL MODELO
#PODRÍAMOS CONFIRMAR LA CORRELACIÓN ENTRE EL PESO FRESCO Y DESCONGELADO CON ESTE MODELO



