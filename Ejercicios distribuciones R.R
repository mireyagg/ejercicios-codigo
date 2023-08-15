#EJERCICIO 3. DISTRIBUCIONES
lam=1.2
#Porcentaje de que haya al menos un mamífero: 69,88%
(1-ppois(0,lambda=lam))
#Probabilidad de que haya 3 animales en la botella: 3,37%
ppois(3,lambda =lam ,lower.tail = FALSE)



#EJERCICIO 6. DISTRIBUCIONES
#10 es el límite superior y 1 es el límite inferior, hay que calcular la probabilidad para ver si entra en el intervalo
pnorm(10,mean=0, sd=5)-pnorm(1,mean=0, sd=5)

#El 39.8% de las desviaciones sobre el peso serán superiores a 1 (se encontrarán entre 1 y 10)



#EJERCICIO 10. DISTRIBUCIONES: se asume que los límites corresponden al 95% de los datos
media=(15+200)/2; desviac=(200-15)/4
pnorm(11,mean = media, sd = desviac)

#RESPUESTA: Sí, se podría tratar de anemia ferropénica porque se encuentra en el
#percentil_1.8 de los datos, por lo que está por debajo del 95%.