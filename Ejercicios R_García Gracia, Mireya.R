dat=read.table(file='data.txt',header=TRUE)

#Ejercicio 1: Una gráfica boxplot de la variable colesterol (Coles) para Sanos – Enfermos (output).
boxplot(dat$coles~dat$output,xlab='Estado del paciente',ylab='Nivel de colesterol')
legend("bottomright",legend=c('sano','enfermo'),pch=c('0','1'))


#Ejercicio 2: Dos histogramas en la misma página que muestren la distribución de la variable creatinina (creat) en sanos y enfermos.
mat=c(1,2)
dim(mat)=c(1,2)
layout(mat)
d1=dat[dat$output==0,]
d2=dat[dat$output==1,]
hist(d1$creat,main='CREATININA EN PACIENTES SANOS',xlab="Niveles de creatinina",ylab="Nº de individuos")
hist(d2$creat,breaks=14,main='CREATININA EN PACIENTES ENFERMOS',xlab="Niveles de creatinina",ylab="Nº de individuos")


#Ejercicio 3: Una gráfica barplot que muestre el porcentaje de sanos y enfermos para aquellos individuos con glucosa (gluc) más alta de 130.
mat1=c(1)
dim(mat1)=c(1)
layout(mat1)
d3=dat[dat$gluc>130,]
counts1=table(d3$output)
pct1=round((counts1/304)*100)
barplot(pct1, main="",xlab="Estado de pacientes", ylab="Individuos con gluc>130 (%)", yaxp = c(0, 100, 5))
legend("topleft",legend=c('sano','enfermo'),pch=c('0','1'))

#Ejercicio 4: Un diagrama circular (piechart) que exprese el porcentaje de los individuos enfermos con las siguientes categorías: Glucosa < 100, Glucosa entre 100 y 125, Glucosa entre 125 y 150 y Glucosa > 150.
dat$glucosa=cut(dat$gluc,c(0,100,125,150,200),labels=c('Glucosa < 100','Glucosa entre 100 y 125','Glucosa entre 125 y 150','Glucosa > 150'))
counts=table(dat$glucosa)
etiquetas=dat$glucosa
pct=round(counts/1000*100)
val=paste(etiquetas, pct,sep=": ") 
val=paste(val,"%",sep="") 
pie(counts,clockwise=TRUE,labels = val,main="Frecuencia de niveles de glucosa",col=rainbow(length(1:4)))


#Ejercicio 5: Una función que calcule la media de los elementos situados por debajo del percentil 60 y entre los percentiles 75 y 95 para un vector cualquiera
d=sample(1:50,size=10)
f1=function(v) {
  s=c(0,0)
  a=c(v>quantile(v,0.75))
  b=c(v<quantile(v,0.95))
  s[1]=mean(v[v<quantile(v,0.60)])
  s[2]=mean(v[a&b])
  return(s)
}
f1(d)
