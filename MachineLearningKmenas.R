data<-res_limpio
data<-data[,-1]
data<-scale(data)
micluster<-kmeans(data, 3, nstart=5, iter.max = 40)
distanciasACententos<-(nrow(data-1)*sum(apply(data,2,var)))
for (i in 2:20) distanciasACententos[i]<-sum(kmeans(data,center=i)$withinss) 
distanciasACententos
plot(1:20, distanciasACententos, type="b", xlab = "Numero Cluster", ylab = "withinss groups")

micluster<-kmeans(data, 8, nstart=5, iter.max = 40)

plot(datosImportanter$Opioniones, datosImportanter$Precio, col=micluster$cluster, xlab = "Opioniones", ylab = "Precio")

library(fmsb)
par(mfrow=c(2,4))

for (i in 1:9) {
  datosImportanter<-as.data.frame(t(micluster$centers[i, ]))
  datosImportanter
  datosImportanter<-rbind(rep(5,10), rep(-1.5,10), datosImportanter)
  datosImportanter
  radarchart(datosImportanter)
}

