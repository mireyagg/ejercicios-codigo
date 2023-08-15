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


# EJERCICIO 1: Intervalo de confianza para positivos y negativos de LM 
# EXPRESAR EL RESULTADO
datos$LMrec[datos$LMcod=="+"]<-"POSITIVO"
datos$LMrec[datos$LMcod=="d"]<-"POSITIVO"
datos$LMrec[datos$LMcod=="-"]<-"NEGATIVO"
datos$LMrec[datos$LMcod=="nd"]<-"NEGATIVO"

t=table(datos$LMrec);t
prop.table(t)*100
barplot(t) 

prop.test(t[[2]], t[[1]]+t[[2]],conf.level=0.95,correct=F)

#RESPUESTA: CON UN 95% DE CONFIANZA, SE PUEDE AFIRMAR QUE EL PORCENTAJE DE POSITIVOS PARA LISTERIA MONOCYTOGENES SE ENCONTRARÁ EN 1.77% Y 8.13%

# EJERCICIO 2: Asociación entre el método de congelación y las posibles detecciones a enfermedades 
datos$SSrec[datos$SScod=="+"]<-"POSITIVO"
datos$SSrec[datos$SScod=="d"]<-"POSITIVO"
datos$SSrec[datos$SScod=="-"]<-"NEGATIVO"
datos$SSrec[datos$SScod=="nd"]<-"NEGATIVO"

datos$OTCrec[datos$OTCcod=="+"]<-"POSITIVO"
datos$OTCrec[datos$OTCcod=="d"]<-"POSITIVO"
datos$OTCrec[datos$OTCcod=="-"]<-"NEGATIVO"
datos$OTCrec[datos$OTCcod=="nd"]<-"NEGATIVO"

#PODEMOS UTILIZAR EL TEST CHI-CUADRADO PARA OBSERVAR LA ASOCIACION O NO DE LAS VARIABLES
t1=table(datos$CONGELACIONcod,datos$OTCrec)  
test <- chisq.test(t1);test
test$observed
test$expected 
test$residuals 
test$stdres 
assocstats(t1)
#P-VALOR = 0.0829, NINGUNA FRECUENCIA ESPERADA MENOR QUE 5, Y LOS RESIDUALES ENTRE -2 Y 2 Y UNA NIVEL DE ASOCIACIÓN DE 0,162 (PEQUEÑA)
# RESPUESTA: CON UN 95% DE PROBABILIDAD, SE PUEDE AFIRMAR EL MÉTODO DE CONGELACIÓN ES INDEPENDIENTE DE LA DETECCIÓN DE OXITETRACICLINA (NO ASOCIADAS)

t2=table(datos$SSrec,datos$CONGELACIONcod)  
test <- chisq.test(t2);test
test$observed
test$expected 
test$residuals 
test$stdres 
assocstats(t2)
#P-VALOR = 0.0035, NINGUNA FRECUENCIA ESPERADA MENOR QUE 5, Y LOS RESIDUALES LIGERAMENTE FUERA DEL LÍMITE ENTRE -2 Y 2 Y UNA NIVEL DE ASOCIACIÓN DE 0,252 (APROXIMADAMENTE MEDIO)
# RESPUESTA: CON UN 95% DE PROBABILIDAD, EL MÉTODO DE CONGELACIÓN ES DEPENDIENTE DE LA DETECCIÓN DE SALMONELLA (EL MÉTODO DE CONGELACIÓN INFLUYE EN EL % DE POSITIVOS Y NEGATIVO PARA SALMONELLA) 

t3=table(datos$LMrec,datos$CONGELACIONcod)  
test <- chisq.test(t3);test
test$observed
test$expected 
test$residuals 
test$stdres 
assocstats(t3)
#P-VALOR = 0.2117, EL 50% DE LAS FRECUENCIAS ESPERADAS SON MENORES QUE 5, Y LOS RESIDUALES SE ENCUENTRAN ENTRE -2 Y 2 Y UNA NIVEL DE ASOCIACIÓN DE 0,133 (PEQUEÑA)
# RESPUESTA: CON UN 95% DE PROBABILIDAD, EL MÉTODO DE CONGELACIÓN ES DEPENDIENTE DE LA DETECCIÓN DE LISTERIA MONOCYTOGENES (EL MÉTODO DE CONGELACIÓN INFLUYE EN EL % DE POSITIVOS Y NEGATIVO PARA LEISHMANIOSIS) 

# EJERCICIO 3: Analizar la normalidad del peso congelado, en el caso Crudo y Cocido
#PRIMERO ES NECESARIO FILTRAR LOS DATOS:
datosCOC=filter(datos, TRAT=="Cocido")
datosCR=filter(datos, TRAT=="Crudo")
dim(datosCOC);dim(datosCR)
#Hay que usar para cocidos el test de Shapiro-Wilk (n=26<50) y para crudos el test de Kolmogorov-Smirnov (n=132>50)

shapiro.test(datosCOC$PESO_CONG)
qqnorm(datosCOC$PESO_CONG, main="Cocidos: peso congelado",pch=20,col = "purple")
qqline(datosCOC$PESO_CONG)
# RESPUESTA: CON UN P-VALOR = 0.0055, SE PUEDE AFIRMAR CON UN 95% DE CONFIANZA QUE EL PESO CONGELADO NO SIGUE UNA DISTRIBUCIÓN NORMAL PARA LOS COCIDOS
#ADICIONALMENTE, ESTO SE CONFIRMA CON EL QQPLOT, YA QUE SE OBSERVAN FUERTES DESVIACIONES EN LOS EXTREMOS DE LA RECTA

ks.test(x = datosCR$PESO_CONG,"pnorm", mean(datosCR$PESO_CONG), sd(datosCR$PESO_CONG))
qqnorm(datosCR$PESO_CONG, main="Crudos: peso fresco",pch=20,col = "blue") 
qqline(datosCR$PESO_CONG)
#RESPUESTA: CON UN P-VALOR = 0.0188, SE PUEDE AFIRMAR CON UN 95% DE CONFIANZA QUE EL PESO CONGELADO NO SIGUE UNA DISTRIBUCIÓN NORMAL PARA LOS CRUDOS
#ADICIONALMENTE, ESTO SE CONFIRMA CON EL QQPLOT, YA QUE SE OBSERVAN DESVIACIONES EN LOS EXTREMOS DE LA RECTA, AUNQUE MÁS SUAVES Y TARDÍAS

#EN EL CASO DEL PESO CONGELADO PARA LOS COCIDOS, SE RECHAZARÍA LA HIPÓTESIS CON MAYOR FUERZA (PRESENTA UN MENOR P-VALOR)



