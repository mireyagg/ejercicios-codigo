#EJERCICIO: Realizar un evaluación mediante validación cruzada con el procedimiento k-fold con 10 categorías con la base de datos “data.txt”.
data<-read.table(file='data.txt',header=T)
ndat<-as.data.frame(scale(data[,1:4],scale=TRUE,center=TRUE))
ndat$output=data$output
library(class)

#1. KNN con k=3
acc1<-vector()
for (i in 1:10) {
  test_index<-seq((i-1)*(nrow(ndat)/10)+1,i*(nrow(ndat)/10))
  testset<-ndat[test_index,]
  trainset<-ndat[-test_index,]
  model1<-knn(train=trainset[,1:4],test=testset[,1:4],cl=trainset[,5],k=3,prob=TRUE)
  tab1<-table(model1,testset[,5])
  acc1[i]=(tab1[1,1]+tab1[2,2])/sum(tab1)
  print(acc1[i])
}
mean(acc1)

#2. KNN con k=10
acc2<-vector()
for (i in 1:10) {
  test_index<-seq((i-1)*(nrow(ndat)/10)+1,i*(nrow(ndat)/10)) #el testindex la vamos a generar con esta fórmula
  testset<-ndat[test_index,]
  trainset<-ndat[-test_index,]
  model2<-knn(train=trainset[,1:4],test=testset[,1:4],cl=trainset[,5],k=10,prob=TRUE)
  tab2<-table(model2,testset[,5])
  acc2[i]=(tab2[1,1]+tab2[2,2])/sum(tab2)
  print(acc2[i])
}
mean(acc2)

#3.Maquina de Soporte de Vectores con kernel “linear”
acc3<-vector()
library(e1071)
for (i in 1:10) {
  test_index<-seq((i-1)*(nrow(ndat)/10)+1,i*(nrow(ndat)/10)) #el testindex la vamos a generar con esta fórmula
  testset<-ndat[test_index,]
  trainset<-ndat[-test_index,]
  svm<-svm(as.factor(output)~.,data=trainset,kernel='linear',probability=TRUE)
  pred<-predict(svm,testset[,1:4],probability=TRUE)
  tab3<-table(pred,testset[,5])
  acc3[i]=(tab3[1,1]+tab3[2,2])/sum(tab3)
  print(acc3[i])
}
mean(acc3) 

#4.Redes Neuronales con un único estrato con 2 neuronas
acc4<-vector()
library(neuralnet)
for (i in 1:10) {
  test_index<-seq((i-1)*(nrow(ndat)/10)+1,i*(nrow(ndat)/10)) #el testindex la vamos a generar con esta fórmula
  testset<-ndat[test_index,]
  trainset<-ndat[-test_index,]
  nn_model<-neuralnet(as.factor(output)~.,data=trainset,hidden=2)
  pred_nn<-compute(nn_model,testset[,1:4])
  pr2<-ifelse(pred_nn$net.result[,2]>0.5,1,0) 
  tab4<-table(pr2,testset[,5])
  acc4[i]=(tab4[1,1]+tab4[2,2])/sum(tab4)
  print(acc4[i])
}
mean(acc4)

#El criterio de comparación es la precisión: 
#KNN k=3: 0.903
#KNN k=10: 0.915
#SVM: 0.845
#Red neuronal: 0.853
#Para los resultados obtenidos al correr una vez el código, el modelo más preciso fue el K-Nearest Neighbours con los 10 datos más cercanos.



