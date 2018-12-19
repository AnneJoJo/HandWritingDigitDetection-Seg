data.lastDense<-read.csv("d:/newresultlabel.csv",header = F)
data.label<-read.csv("d:/inputdata.csv", header= F)
data.lastDense<-cbind(data.lastDense,data.label)
#data.lastDense<-read.csv("d:/dense128.csv",header = T)
#data.lastDense<-read.csv("d:/max.csv",header = T)
#data.lastDense<-fread("d:/predict_activations.csv",header = T,data.table = F)
install.packages('proxy') 
library('proxy')
install.packages('SnowballC')
library('lsa')
library(data.table)
#data.lastDense=fread("d:/c4.csv",data.table = F)
#data.lastDense=fread("d:/max4.csv",data.table = F)
#############################################SAME pair#####################################
##################45*C(2,1500~)#################(Aevrgae first)
memory.limit(15000)

# sort:
data.sort<-data.lastDense[order(data.lastDense$V1),]
data.sort<-data.sort[order(data.sort$V2),] 
data.sort$label<-NA
for(i in 1:nrow(data.sort)){
  dd=data.sort$V1[i]+data.sort$V2[i]*10
  data.sort$label[i]=as.character(dd)
}

#table(data.sort$label)
label.m<-data.frame()

same<-data.frame()

for (i in 0:8){
  n=i+1
  for(j in n:9){
    number.t=j*10+i
    label.m<-rbind(label.m,number.t)
    temp.d<-data.sort[which(data.sort$label==number.t),c(3:786)]
    d.same.dist=dist(as.matrix(temp.d),method = "euclidean")
    #d.same.dist=dist(as.matrix(temp.d),method = "cosine")
    d.same.ad=sum(d.same.dist)/length(d.same.dist)
    same<-rbind(same,d.same.ad)
    
  }
}
###############################Share One##########################################
########################10*C(2,9)*1500~

#Average matrix:
Mean<-data.frame()
label.m<-data.frame()
for (i in 0:8){
  n=i+1
  for(j in n:9){
    number.t=j*10+i
    label.m<-rbind(label.m,number.t)
    temp.m<-data.sort[which(data.sort$label==number.t),-787]
    temp.av<-apply(temp.m,2,mean)
    Mean<-rbind(Mean,temp.av)
  }
}

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==0),]
d.diff.2<-Mean[which(Mean$X1==0),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")
name<-c("01","02","03","04","05","06","07","08","09")

#diff.d0.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d0.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d0.list<-as.data.frame.list(diff.d0.dist)
diff.d0.list<-t(diff.d0.list)

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==1),]
d.diff.2<-Mean[which(Mean$X1==1),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")


#diff.d1.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d1.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d1.list<-as.data.frame.list(diff.d1.dist)
diff.d1.list<-t(diff.d1.list)

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==2),]
d.diff.2<-Mean[which(Mean$X1==2),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")

#diff.d2.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d2.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d2.list<-as.data.frame.list(diff.d2.dist)
diff.d2.list<-t(diff.d2.list)

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==3),]
d.diff.2<-Mean[which(Mean$X1==3),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")

#diff.d3.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d3.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d3.list<-as.data.frame.list(diff.d3.dist)
diff.d3.list<-t(diff.d3.list)

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==4),]
d.diff.2<-Mean[which(Mean$X1==4),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")

#diff.d4.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d4.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d4.list<-as.data.frame.list(diff.d4.dist)
diff.d4.list<-t(diff.d4.list)

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==5),]
d.diff.2<-Mean[which(Mean$X1==5),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")

#diff.d5.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d5.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d5.list<-as.data.frame.list(diff.d5.dist)
diff.d5.list<-t(diff.d5.list)

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==6),]
d.diff.2<-Mean[which(Mean$X1==6),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")

#diff.d6.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d6.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d6.list<-as.data.frame.list(diff.d6.dist)
diff.d6.list<-t(diff.d6.list)

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==7),]
d.diff.2<-Mean[which(Mean$X1==7),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")

#diff.d7.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d7.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d7.list<-as.data.frame.list(diff.d7.dist)
diff.d7.list<-t(diff.d7.list)

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==8),]
d.diff.2<-Mean[which(Mean$X1==8),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")

#diff.d8.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d8.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d8.list<-as.data.frame.list(diff.d8.dist)
diff.d8.list<-t(diff.d8.list)

d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-Mean[which(Mean$X0==9),]
d.diff.2<-Mean[which(Mean$X1==9),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$X1),]
colnames(d.diff)<-c("d")

#diff.d9.dist=dist(as.matrix(d.diff),method = "cosine")
diff.d9.dist=dist(as.matrix(d.diff),method = "euclidean")
diff.d9.list<-as.data.frame.list(diff.d9.dist)
diff.d9.list<-t(diff.d9.list)
diff.ad<-c(diff.d0.list,diff.d1.list,diff.d2.list,diff.d3.list,diff.d4.list,diff.d5.list,diff.d6.list,diff.d7.list,diff.d8.list,diff.d9.list)
diff.ad<-as.data.frame(diff.ad)
###############################################DIFF Both######################################
###############################((45*(45-9-9+1))/2)*1500~

diff.all<-data.frame()
for(i in 1:nrow(Mean)){
  for (j in 1:nrow(Mean)){
    if (Mean$X0[i]!=Mean$X0[j]&& Mean$X0[i]!=Mean$X1[j] && Mean$X1[i]!=Mean$X0[j]&& Mean$X1[i]!=Mean$X1[j]){
      
      
      #dd<-Eucli(Mean[i,-c(1:2)],Mean[j,-c(1:2)])
      #x=as.numeric(Mean[i,-c(1:2)])
      #y=as.numeric(Mean[j,-c(1:2)])
      M<-rbind(Mean[i,-c(1:2)],Mean[j,-c(1:2)])
      #dd<-dist(as.matrix(M),method = "cosine")
      dd<-dist(as.matrix(M),method = "euclidean")
      #dd<-cosine(x,y)
      diff.all<-rbind(diff.all,dd)
    }
  }
}
#######################################BOXPLOT#############################################
data.b<-data.frame()
same[46:1260,]<-NA
diff.ad[361:1260,]<-NA
data.b<-cbind(same,diff.ad,diff.all)
colnames(data.b)<-c("Same Digit","Half Same","Different Digit")

boxplot(na.omit(data.b),col=(c("gold","darkgreen","red")),main="Max_lastlayer", ylab="Average Distance(Euc)")
write.csv(data.b,file = "denseoutput_e.csv")
