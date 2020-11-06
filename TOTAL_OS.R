library(neuralnet)
library(tseries)
library(forecast)
library(readxl)
data <- read_excel("D:/A.Tugas Akhir/BAB 4/SBY/DNN/DATA DNN OS.xlsx", 
                   sheet = "TOTAL")
datat<-as.ts(data)
training<-as.ts(datat[1:96,])
testing<-as.ts(datat[97:114,])

ystd<-(training[,1]-min(training[,1]))/(max(training[,1])-min(training[,1]))

#lag yang signifikan
lagyt<-c(1,2,3,4,12,13,14,15,16)
maxlag<-max(lagyt)
laginput<-matrix(NA,(length(ystd)-maxlag),length(lagyt))

for (j in 1:length(lagyt))
{
  laginput[,j]<-ystd[(maxlag-lagyt[j]+1):(length(ystd)-lagyt[j])]
}
head(laginput)

colnames(laginput)<-c("Yt_1","Yt_2","Yt_3","Yt_4","Yt_12","Yt_13","Yt_14","Yt_15","Yt_16")
Yt<-ystd[(maxlag+1):length(ystd)]

TOTAL<-data.frame(cbind(Yt,laginput,datat[(maxlag+1):length(ystd),-1]))
head(TOTAL)

#jumlah neuron
h1<-c(1,2,3,4,5)
h2<-c(1,2,3,4,5)
neuron<-cbind(rep(h1,times=length(h2)), rep(h2,each=length(h1)))
n_fore<-18
dummy1<-rbind((datat[(maxlag+1):length(ystd),-1]),testing[,-1])

koefisien=list()
bestmodel=list()
fitsmodel=list()
foremodel=list()
akurasi=list()

for (k in seq_along(neuron[,1]))
{
  set.seed(k)
  best.model<-neuralnet(Yt~Yt_1+Yt_2+Yt_3+Yt_4+Yt_12+Yt_13+Yt_14+Yt_15+Yt_16+t+M_1+M_2+M_3+M_4+M_5+M_6+M_7+M_8+
                          M_9+M_10+M_11+M_12+V1t+V1t1+V2t+V2t1+V3t+V3t1+V4t+V4t1, 
                        data=TOTAL,hidden=neuron[k,],stepmax = 1e+08,rep=10,
                        act.fct = "logistic", linear.output = TRUE, likelihood = TRUE)
  plot(best.model, rep="best")
  mypath<-file.path("D:/A.Tugas Akhir/BAB 4/SBY/DNN/PLOT/TOTAL OS", paste("Total OS",(k),".jpg",sep=""))
  png(file=mypath)
  plot(best.model,rep="best")
  dev.off()
  
  koefisien[[k]]<-best.model$weight[which.min(best.model$result.matrix[1,])]
  
  ytesting<-c(TOTAL[,1],rep(0,n_fore))
  
  for (l in ((length(TOTAL[,1])+1):(length(TOTAL[,1])+n_fore)))
  {
    dummytesting<-t(as.matrix(dummy1[l,]))
    lagtesting<-matrix(0,1,length(lagyt))
    for (m in 1:length(lagyt))
    {
      lagtesting[[m]]<-ytesting[(l-lagyt[m])]
    }
    inputtesting<-cbind(lagtesting,dummytesting)
    ytesting[l]<-compute(best.model,covariate = inputtesting,
                         rep = which.min(best.model$result.matrix[1,]))$net.result
  }
  
  fits.model<-unlist(best.model$net.result[which.min(best.model$result.matrix[1,])])*
    (max(training[,1])-min(training[,1]))+min(training[,1])
  NtTrain<-(TOTAL[,1]*(max(training[,1])-min(training[,1]))+min(training[,1]))-
    (unlist(best.model$net.result[which.min(best.model$result.matrix[1,])])*
       (max(training[,1])-min(training[,1]))+min(training[,1]))
  AvgNtTrain<-(abs(TOTAL[,1]*(max(training[,1])-min(training[,1]))+min(training[,1]))+
                 abs(unlist(best.model$net.result)*(max(training[,1])-min(training[,1]))+
                       min(training[,1])))/2
  RMSETraining<-((sum(NtTrain^2))/length(NtTrain))^(1/2)
  
  sMAPE1<-c()
  for (p1 in 1:length(NtTrain))
  {
    sMAPE1[p1]<-(1/length(NtTrain))*abs(NtTrain[p1])/AvgNtTrain[p1]
  }
  sMAPETraining<-sum(sMAPE1)*100
  
  fore.model<-ytesting[(length(TOTAL[,1])+1):(length(TOTAL[,1])+n_fore)]*(max(training[,1])-min(training[,1]))+min(training[,1])
  NtTest<-(testing[,1])-(ytesting[(length(TOTAL[,1])+1):(length(TOTAL[,1])+n_fore)]*(max(training[,1])-min(training[,1]))+min(training[,1]))
  AvgNtTest<-(abs(testing[,1]+abs(ytesting[(length(TOTAL[,1])+1):(length(TOTAL[,1])+n_fore)]*(max(training[,1])-min(training[,1]))+min(training[,1])))/2)
  RMSETesting<-((sum(NtTest^2))/length(NtTest))^(1/2)
  
  sMAPE2<-c()
  for (p2 in 1:length(NtTest))
  {
    sMAPE2[p2]<-(1/length(NtTest))*abs(NtTest[p2])/AvgNtTest[p2]
  }
  sMAPETesting<-sum(sMAPE2)*100
  
  
  fitsmodel[[k]]<-fits.model
  foremodel[[k]]<-fore.model
  akurasi[[k]]<-rbind(RMSETraining,RMSETesting,sMAPETraining,sMAPETesting)
}

write.csv(akurasi,"D:/A.Tugas Akhir/BAB 4/SBY/DNN/OUTPUT/Total OS Akurasi.csv")
write.csv(fitsmodel,"D:/A.Tugas Akhir/BAB 4/SBY/DNN/OUTPUT/Total OS Fits.csv")
write.csv(foremodel,"D:/A.Tugas Akhir/BAB 4/SBY/DNN/OUTPUT/Total OS Fore.csv")
saveRDS(koefisien,file = "D:/A.Tugas Akhir/BAB 4/SBY/DNN/KOEF/Total OS Koef.rds")
