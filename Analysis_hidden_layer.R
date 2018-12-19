####################Load Data############################################
#########################################################################
library(data.table)
#data.lastDense<-fread("d:/predict_activations.csv",header = T,data.table = F)
#data.lastDense<-read.csv("d:/dense128.csv",header = F)
#data.lastDense<-read.csv("d:/auto2.csv",header = T)
#data.lastDense<-read.csv("d:/cov2.csv",header = T)
#data.lastDense<-read.csv("d:/te_results.csv",header = F)
#data.labels<-read.csv("d:/act_results.csv",head = F)
#data.lastDense<-cbind(data.labels,data.lastDense)

#data.lastDense<-read.csv("d:/Firstinputtest.csv",header = T)

library(data.table)
data.lastDense=fread("d:/c4.csv",data.table = F)

#library(data.table)
#data.lastDense=fread("d:/c3.csv",data.table = F)

install.packages('proxy') #
library('proxy')
library('lsa')

################################Function section##########################
##########################################################################
### Chi-plot:

chisplot <- function(x) {
  if (!is.matrix(x)) stop("x is not a matrix")
  
  ### determine dimensions
  n <- nrow(x)
  p <- ncol(x)
  #
  xbar <- apply(x, 2, mean)
  S <- var(x)
  S <- base::solve(S)
  index <- (1:n)/(n+1)
  #
  xcent <- t(t(x) - xbar)
  di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S)
  #
  quant <- qchisq(index,p)
  plot(quant, base::sort(di), ylab = "Ordered distances",
       xlab = "Chi-square quantile", lwd=2,pch=1)
  
}

### IMage:

myImagePlot <- function(x, ...){
  min <- min(x)
  max <- max(x)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  ColorRamp <- rgb( seq(1,0,length=256),  # Red
                    seq(1,0,length=256),  # Green
                    seq(0,1,length=256))  # Blue
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="Correct",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}
Eucli<-function(x,y){
  s = 0
  for (i in 1:length(x)){
    s=s+(x[i]-y[i])^2
  }
  e = sqrt(s)
  return(e)
}

samepair<-function(x){
  x=as.data.frame(x)
  container<-data.frame()
  for(i in 1:(nrow(x)-1)){
    n=i+1
    for(j in n:nrow(x)){
      x=as.numeric(x[i,])
      y=as.numeric(x[j,])
      temp=cosine(x,y)
      container<-rbind(container,temp)
    }
  }
  ret=sum(container)/length(container)
  return(ret)
} 
#################################Data manipulation###########################
#############################################################################

memory.limit(15000)

# sort:
data.sort<-data.lastDense[order(data.lastDense$D1),]
data.sort<-data.sort[order(data.sort$D2),] 
data.sort$label<-NA
for(i in 1:nrow(data.sort)){
  dd=data.sort$D1[i]+data.sort$D2[i]*10
  data.sort$label[i]=as.character(dd)
}


##################################################
############same digit ##########################
table(data.sort$label)
label.m<-data.frame()
same<-data.frame()

for (i in 0:8){
  n=i+1
  for(j in n:9){
    number.t=j*10+i
    label.m<-rbind(label.m,number.t)
    temp.d<-data.sort[which(data.sort$label==number.t),c(4:787)]
    #d.same.ad<-samepair(temp.d)
    d.same.dist=dist(as.matrix(temp.d),method = "cosine")
    d.same.ad=sum(d.same.dist)/length(d.same.dist)
    same<-rbind(same,d.same.ad)
   
  }
}


###############test#################################################
data.01<-data.sort[which(data.sort$label=="10"),c(4:787)]
samepair(data.01)

x=as.numeric(data.01[1,])
y=as.numeric(data.01[3,])
cosine(x,y)
Eucli(x,y)

container<-data.frame()
for(i in 1:nrow(data.01)){
  n=i+1
  for(j in n:(nrow(data.01))){
    x=as.numeric(data.01[i,])
    y=as.numeric(data.01[j,])
    temp=cosine(x,y)
    container<-rbind(container,temp)
  }
}
container/length(container)

Mean<-data.frame()
label.m<-data.frame()
for (i in 0:8){
  n=i+1
  for(j in n:9){
    number.t=j*10+i
    label.m<-rbind(label.m,number.t)
    temp.m<-data.sort[which(data.sort$label==number.t),c(3:786)]
    temp.av<-apply(temp.m,2,mean)
    Mean<-rbind(Mean,temp.av)
  }
}
#Mean<-cbind(Mean,label.m) 

a=Mean[c(1,45),]
x=as.numeric(a[1,])
y=as.numeric(a[2,])
cosine(x,y)
Eucli(x,y)
###############################################################################

##################### One Digit Same##########################
############################0################################
d.diff<-data.frame()
d.diff.all<-data.frame()
d.diff<-data.lastDense[which(data.lastDense$D1==0),]
d.diff.2<-data.lastDense[which(data.lastDense$D2==0),]
d.diff<-rbind(d.diff,d.diff.2)
d.diff<-d.diff[order(d.diff$D2),]
 
Mean.0<-data.frame()
   for(j in 1:9){
     temp.m<-d.diff[which(d.diff$D2==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.0<-rbind(Mean.0,temp.av)
   }
 colnames(Mean.0)<-c("d")
 name<-c("01","02","03","04","05","06","07","08","09")
 row.names(Mean.0)<-t(as.vector(name))
 diff.d0.dist=dist(Mean.0,method = "cosine")
 diff.d0.list<-as.data.frame.list(diff.d0.dist)
 diff.d0.list<-t(diff.d0.list)
 #diff.d0.ad<-sum(diff.d0.dist)/length(diff.d0.dist)
##################################################################
 #######################1#########################################

 d.diff<-data.frame()
 d.diff.all<-data.frame()

 d.diff<-data.lastDense[which(data.lastDense$D1==1),]
 d.diff.2<-data.lastDense[which(data.lastDense$D2==1),]
 d.diff<-rbind(d.diff,d.diff.2)
 d.diff<-d.diff[order(d.diff$D2),]
 
 Mean.1<-data.frame()
 label.m<-data.frame()
 
 for(j in 1:9){
   temp.m<-d.diff[which(d.diff$D2==j),c(4:787)]
   temp.av<-apply(temp.m,2,mean)
   Mean.1<-rbind(Mean.1,temp.av)
 }
 colnames(Mean.1)<-c("d")
 name<-c("01","12","13","14","15","16","17","18","19")
 row.names(Mean.1)<-t(as.vector(name))
 diff.d1.dist=dist(Mean.1,method = "cosine")
 diff.d1.list<-as.data.frame.list(diff.d1.dist)
 diff.d1.list<-t(diff.d1.list)
 #diff.d1.ad<-sum(diff.d1.dist)/length(diff.d1.dist)
######################################2##############################
 d.diff<-data.frame()
 d.diff<-data.lastDense[which(data.lastDense$D1==2),]
 d.diff.2<-data.lastDense[which(data.lastDense$D2==2),]
 d.diff<-rbind(d.diff,d.diff.2)
 d.diff<-d.diff[order(d.diff$D1),]
 d.diff<-d.diff[order(d.diff$D2),]
 Mean.2<-data.frame()
 
 for(j in 0:9){
   if (j < 2){
     temp.m<-d.diff[which(d.diff$D1==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.2<-rbind(Mean.2,temp.av)
   }
   else if (j >2){
   temp.m<-d.diff[which(d.diff$D2==j),c(4:787)]
   temp.av<-apply(temp.m,2,mean)
   Mean.2<-rbind(Mean.2,temp.av)
   }
 }
 colnames(Mean.2)<-c("d")
 name<-c("02","12","23","24","25","26","27","28","29")
 row.names(Mean.2)<-t(as.vector(name))
 diff.d2.dist=dist(Mean.2,method = "cosine")
 diff.d2.list<-as.data.frame.list(diff.d2.dist)
 diff.d2.list<-t(diff.d2.list)
 #diff.d2.ad<-sum(diff.d2.dist)/length(diff.d2.dist)
 
############################3####################################
 d.diff<-data.frame()
 d.diff<-data.lastDense[which(data.lastDense$D1==3),]
 d.diff.2<-data.lastDense[which(data.lastDense$D2==3),]
 d.diff<-rbind(d.diff,d.diff.2)
 d.diff<-d.diff[order(d.diff$D1),]
 d.diff<-d.diff[order(d.diff$D2),]
 
 Mean.3<-data.frame()
 
 for(j in 0:9){
   if (j < 3){
     temp.m<-d.diff[which(d.diff$D1==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.3<-rbind(Mean.3,temp.av)
   }
   else if (j >3){
     temp.m<-d.diff[which(d.diff$D2==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.3<-rbind(Mean.3,temp.av)
   }
 }
 colnames(Mean.3)<-c("d")
 name<-c("03","13","23","34","35","36","37","38","39")
 row.names(Mean.3)<-t(as.vector(name))
 diff.d3.dist=dist(Mean.3,method = "cosine")
 diff.d3.list<-as.data.frame.list(diff.d3.dist)
 diff.d3.list<-t(diff.d3.list)
 #diff.d3.ad<-sum(diff.d3.dist)/length(diff.d3.dist)
 ############################4##################################
 d.diff<-data.frame()
 d.diff<-data.lastDense[which(data.lastDense$D1==4),]
 d.diff.2<-data.lastDense[which(data.lastDense$D2==4),]
 d.diff<-rbind(d.diff,d.diff.2)
 d.diff<-d.diff[order(d.diff$D1),]
 d.diff<-d.diff[order(d.diff$D2),]
 
 Mean.4<-data.frame()
 
 for(j in 0:9){
   if (j < 4){
     temp.m<-d.diff[which(d.diff$D1==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.4<-rbind(Mean.4,temp.av)
   }
   else if (j >4){
     temp.m<-d.diff[which(d.diff$D2==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.4<-rbind(Mean.4,temp.av)
   }
 }
 colnames(Mean.4)<-c("d")
 name<-c("04","14","24","34","45","46","47","48","49")
 row.names(Mean.4)<-t(as.vector(name))
 diff.d4.dist=dist(Mean.4,method = "cosine")
 diff.d4.list<-as.data.frame.list(diff.d4.dist)
 diff.d4.list<-t(diff.d4.list)
 #diff.d4.ad<-sum(diff.d4.dist)/length(diff.d4.dist)
 ###############################5#############################
 d.diff<-data.frame()
 d.diff<-data.lastDense[which(data.lastDense$D1==5),]
 d.diff.2<-data.lastDense[which(data.lastDense$D2==5),]
 d.diff<-rbind(d.diff,d.diff.2)
 d.diff<-d.diff[order(d.diff$D1),]
 d.diff<-d.diff[order(d.diff$D2),]
 
 Mean.5<-data.frame()
 
 for(j in 0:9){
   if (j < 5){
     temp.m<-d.diff[which(d.diff$D1==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.5<-rbind(Mean.5,temp.av)
   }
   else if (j >5){
     temp.m<-d.diff[which(d.diff$D2==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.5<-rbind(Mean.5,temp.av)
   }
 }
 colnames(Mean.5)<-c("d")
 name<-c("05","15","25","35","45","56","57","58","59")
 row.names(Mean.5)<-t(as.vector(name))
 diff.d5.dist=dist(Mean.5,method = "cosine")
 diff.d5.list<-as.data.frame.list(diff.d5.dist)
 diff.d5.list<-t(diff.d5.list)
 #diff.d5.ad<-sum(diff.d5.dist)/length(diff.d5.dist)

#####################################6##############################
 d.diff<-data.frame()
 d.diff<-data.lastDense[which(data.lastDense$D1==6),]
 d.diff.2<-data.lastDense[which(data.lastDense$D2==6),]
 d.diff<-rbind(d.diff,d.diff.2)
 d.diff<-d.diff[order(d.diff$D1),]
 d.diff<-d.diff[order(d.diff$D2),]
 
 Mean.6<-data.frame()
 
 for(j in 0:9){
   if (j < 6){
     temp.m<-d.diff[which(d.diff$D1==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.6<-rbind(Mean.6,temp.av)
   }
   else if (j >6){
     temp.m<-d.diff[which(d.diff$D2==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.6<-rbind(Mean.6,temp.av)
   }
 }
 colnames(Mean.6)<-c("d")
 name<-c("06","16","26","36","46","56","67","68","69")
 row.names(Mean.6)<-t(as.vector(name))
 diff.d6.dist=dist(Mean.6,method = "cosine")
 diff.d6.list<-as.data.frame.list(diff.d6.dist)
 diff.d6.list<-t(diff.d6.list)
 #diff.d6.ad<-sum(diff.d6.dist)/length(diff.d6.dist)
 #d.diff.all<-rbind(d.diff.all,d.diff)
 
#}
###############################7####################################
 d.diff<-data.frame()
 d.diff<-data.lastDense[which(data.lastDense$D1==7),]
 d.diff.2<-data.lastDense[which(data.lastDense$D2==7),]
 d.diff<-rbind(d.diff,d.diff.2)
 d.diff<-d.diff[order(d.diff$D1),]
 d.diff<-d.diff[order(d.diff$D2),]
 
 Mean.7<-data.frame()
 
 for(j in 0:9){
   if (j < 7){
     temp.m<-d.diff[which(d.diff$D1==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.7<-rbind(Mean.7,temp.av)
   }
   else if (j >7){
     temp.m<-d.diff[which(d.diff$D2==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.7<-rbind(Mean.7,temp.av)
   }
 }
 colnames(Mean.7)<-c("d")
 name<-c("07","17","27","37","47","57","67","78","79")
 row.names(Mean.7)<-t(as.vector(name))
 diff.d7.dist=dist(Mean.7,method = "cosine")
 diff.d7.list<-as.data.frame.list(diff.d7.dist)
 diff.d7.list<-t(diff.d7.list)
# diff.d7.ad<-sum(diff.d7.dist)/length(diff.d7.dist)
 ################################8##############################
 d.diff<-data.frame()
 d.diff<-data.lastDense[which(data.lastDense$D1==8),]
 d.diff.2<-data.lastDense[which(data.lastDense$D2==8),]
 d.diff<-rbind(d.diff,d.diff.2)
 d.diff<-d.diff[order(d.diff$D1),]
 d.diff<-d.diff[order(d.diff$D2),]
 
 Mean.8<-data.frame()
 
 for(j in 0:9){
   if (j < 8){
     temp.m<-d.diff[which(d.diff$D1==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.8<-rbind(Mean.8,temp.av)
   }
   else if (j >8){
     temp.m<-d.diff[which(d.diff$D2==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.8<-rbind(Mean.8,temp.av)
   }
 }
 colnames(Mean.8)<-c("d")
 name<-c("08","18","28","38","48","58","68","78","89")
 row.names(Mean.8)<-t(as.vector(name))
 diff.d8.dist=dist(Mean.8,method = "cosine")
 diff.d8.list<-as.data.frame.list(diff.d8.dist)
 diff.d8.list<-t(diff.d8.list)
 #diff.d8.ad<-sum(diff.d8.dist)/length(diff.d8.dist)
 ##################################9#################################
 d.diff<-data.frame()
 d.diff<-data.lastDense[which(data.lastDense$D1==9),]
 d.diff.2<-data.lastDense[which(data.lastDense$D2==9),]
 d.diff<-rbind(d.diff,d.diff.2)
 d.diff<-d.diff[order(d.diff$D1),]
 d.diff<-d.diff[order(d.diff$D2),]
 
 Mean.9<-data.frame()
 
 for(j in 0:8){
   
     temp.m<-d.diff[which(d.diff$D1==j),c(4:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean.9<-rbind(Mean.9,temp.av)
  
 }
 colnames(Mean.9)<-c("d")
 name<-c("09","19","29","39","49","59","69","79","89")
 row.names(Mean.9)<-t(as.vector(name))
 diff.d9.dist=dist(Mean.9,method = "cosine")
 diff.d9.list<-as.data.frame.list(diff.d9.dist)
 diff.d9.list<-t(diff.d9.list)
 #diff.d9.list<-as.data.frame(t(diff.d9.list))
 #diff.d9.ad<-sum(diff.d9.dist)/length(diff.d9.dist)
 
 diff.ad<-c(diff.d0.list,diff.d1.list,diff.d2.list,diff.d3.list,diff.d4.list,diff.d5.list,diff.d6.list,diff.d7.list,diff.d8.list,diff.d9.list)
 diff.ad<-as.data.frame(diff.ad)
 ###########################Both Digit are different####################
 #######################################################################
 
 #Average matrix:
 Mean<-data.frame()
 label.m<-data.frame()
 for (i in 0:8){
   n=i+1
   for(j in n:9){
     number.t=j*10+i
     label.m<-rbind(label.m,number.t)
     temp.m<-data.sort[which(data.sort$label==number.t),c(2:787)]
     temp.av<-apply(temp.m,2,mean)
     Mean<-rbind(Mean,temp.av)
   }
 }
 #Mean<-cbind(Mean,label.m) 
 
 #Mean.01<-Mean[1,]
 #Mean.other<-Mean[which(Mean$X0!=0 && Mean$X1=)]
 x=as.numeric(Mean[1,-c(1:2)])
 y=as.numeric(Mean[40,-c(1:2)])
 M<-rbind(Mean[1,-c(1:2)],Mean[40,-c(1:2)])
 dd<-dist(M,method = "cosine")
 
 diff.all<-data.frame()
 for(i in 1:nrow(Mean)){
   for (j in 1:nrow(Mean)){
     if (Mean$X0[i]!=Mean$X0[j]&& Mean$X0[i]!=Mean$X1[j] && Mean$X1[i]!=Mean$X0[j]&& Mean$X1[i]!=Mean$X1[j]){
       i
       j
       #dd<-Eucli(Mean[i,-c(1:2)],Mean[j,-c(1:2)])
       #x=as.numeric(Mean[i,-c(1:2)])
       #y=as.numeric(Mean[j,-c(1:2)])
       M<-rbind(Mean[i,-c(1:2)],Mean[j,-c(1:2)])
       dd<-dist(M,method = "cosine")
       #dd<-cosine(x,y)
       diff.all<-rbind(diff.all,dd)
     }
   }
 }
##########################For the boxplot
 
 data.b<-data.frame()

 same[46:1260,]<-NA
 #diff.ad<-as.data.frame(diff.ad)
 diff.ad[361:1260,]<-NA
 data.b<-cbind(same,diff.ad,diff.all)
 #data.b<-cbind(same,diff.all)
 colnames(data.b)<-c("Same Digit","Half Same","Different Digit")
 
 boxplot(na.omit(data.b),col=(c("gold","darkgreen","red")),main="Input_layer", ylab="Average Distance(Cos)")

write.csv(data.b,file = "input.csv")
 
 #################################################################################

data.summ=read.csv("d:/input.csv")
data.summ<-data.summ[,-1]
boxplot(na.omit(data.summ),col=(c("gold","gold","gold","darkgreen","darkgreen","darkgreen","red","red","red")),main="Transition of hidden layers", ylab="Average Distance(Eucl)")


##############################################################################

d.diff.ad<-data.frame()
for (i in 1:nrow(d.diff)){
  for (j in 1:nrow(d.diff)){
  if (d.diff$D1[i]!=d.diff$D1[j]&d.diff$D2[i]!=d.diff$D2[j]){
    temp<-Eucli(d.diff[i,-c(1:3)],d.diff[j,-c(1:3)])
    d.diff.ad<-rbind(d.diff.ad,temp)
  }
  }
} 




d.01<-data.sort[1:1618,c(4:131)]
d.01<-as.matrix(d.01)
d.01.dist=dist(d.01)
d.01.ad=sum(d.01.dist)/length(d.01.dist)






#tempmat<-a.01[sample(1618),] # shuffle the matrix


m= as.matrix(m) # should calculate average here.
myImagePlot(m)

boxplot(m)


label.m<-data.frame()
for (i in 0:8) { 
  n=i+1
  for(j in n:9){
    number.t=j*10+i
    label.m<-rbind(label.m,number.t)
    temp.m<-data.sort[which(data.sort$label==number.t),c(3:130)]
    a.inner<-as.matrix(temp.m)
    d.inner<- as.matrix(dist(a.inner))
  
  }
}



#Average matrix:
Mean<-data.frame()
label.m<-data.frame()
for (i in 0:8){
  n=i+1
  for(j in n:9){
    number.t=j*10+i
    label.m<-rbind(label.m,number.t)
    temp.m<-data.sort[which(data.sort$label==number.t),c(4:131)]
    temp.av<-apply(temp.m,2,mean)
    Mean<-rbind(Mean,temp.av)
  }
    
}

Mean<-cbind(Mean,label.m)
#colnames(Mean)<-c("d")
discompute<- function(x,y){
  sum = 0
  ret = 0
  for (i in 1:length(x)){
    sum=sum+(x[i]-y[i])*(x[i]-y[i])
  }
  ret=sqrt(sum)
  return(ret)
}

x<-c(2,3,4)
y<-c(3,4,5)

discompute(x,y)

distance<-data.frame()
for (n in 1:nrow(Mean)){ 
  for( m in 1:nrow(a.1.df)){
    temp.d=0
    x<-as.numeric(Mean[n,])
    y<-as.numeric(a.1[m,])
    temp.d<-discompute(x,y) ##??
    distance<-rbind(distance,temp.d)
    
  }
}
  



# filter:
temp.for.operation<-data.sort
#temp.for.operation<-score
data.sort.filter<-data.frame()
labelFreq<-table(data.sort$label)
labelFreq<-as.data.frame(labelFreq)
start=1
for(i in 1:nrow(labelFreq)){
  temp<-temp.for.operation[which(temp.for.operation$label ==labelFreq$Var1[i]),]
  cou<-floor(labelFreq$Freq[i]/10)
  ends<-start+cou
  temp<-temp[start:ends,]
  data.sort.filter<-rbind(data.sort.filter,temp)
  #start=start+labelFreq$Freq
  
}
#########################MDS#######################################
####################################################################
data.m.dis<-dist(as.matrix(Mean[,-3]))

data.mds<-cmdscale(data.m.dis,k=2,eig=T)
summary(data.mds)

mdsscore<-as.data.frame(data.mds$points)

mdsscore<-cbind(mdsscore,Mean[,3])

ggplot(data = mdsscore,aes(x=mdsscore[,1] , y= mdsscore[,2]))+
  geom_point()+
  geom_text(aes(label=mdsscore[,3]),position = "identity",hjust=0, vjust=0)+
  labs(title="Cov layer_2 Average Distance MDS ")+xlab("Score1")+ylab("Score2")+
  theme_bw()

###################################Average Distaince###########################
########################Euclidean and Cosine Similarity########################

##Eucl:
data.m.dis<-as.matrix(data.m.dis)
colnames(data.m.dis)<-c(t(label.m))
row.names(data.m.dis)<-c(t(label.m))
myImagePlot(data.m.dis,title="Cov layer_3 average distance between each pair")
##Cosine:
library(lsa)
data.cos.dis<-cosine(t(as.matrix(Mean[,-3])))
colnames(data.cos.dis)<-c(t(label.m))
row.names(data.cos.dis)<-c(t(label.m))
myImagePlot(data.cos.dis,title="Cov layer_3 average distance between each pair(Cosine)")







###########################Visualization Section########################
########################################################################
#data.sort.number<-data.sort[,-c(1:3,6)]
labels<-data.sort$label
data.sort.number.lab<-data.sort[,-c(1,131)]
data_digit_0<-data.sort.number.lab[which(data.sort.number.lab$D1==0),]
Mean.lab<-list()

for(k in 1:9){ 
  #label_0[k]<- 10*k
  temp.m<-data.sort[which(data.sort.number.lab$D2== k),c(3:132)]
  temp_0<-apply(temp.m,2,mean)
  Mean.lab<-rbind(Mean.lab,temp_0)
  
  
}

row.names(Mean.lab)<-c('01', '02','03','04','05','06','07','08','09')
a=as.matrix(Mean.lab[,-1])
d =as.matrix(dist(a))
myImagePlot(d)


data_digit_1<-data.sort.number.lab[which(data.sort.number.lab$D1==1),]
Mean.lab.1<-list()
for(k in 2:9){ 
  temp.m<-data.sort[which(data.sort.number.lab$D2== k),c(4:131)]
  temp_1<-apply(temp.m,2,mean)
  Mean.lab.1<-rbind(Mean.lab.1,temp_1)
  
}
row.names(Mean.lab.1)<-c('12','13','14','15','16','17','18','19')
#a.1.df<-as.data.frame(Mean.lab.1[,-1])
a.1=as.matrix(Mean.lab.1)
d.1 =as.matrix(dist(a.1))
myImagePlot(d.1)






data.dist<-dist(data.sort.number)
data.mds<-cmdscale(data.dist,k=2,eig=T)
summary(data.mds)


plot(data.sort[,4],data.sort[,5],type='n',ylab="x",xlab="x2",col=labels)
text(data.sort[,4],data.sort[,5],labels = labels)

ggplot(data = data.sort)+
  geom_point(aes(data.sort[,4],data.sort[,5]),color=data.sort[,6],position = "identity")+
  theme_bw()
ggplot(data = data.sort,aes(x=data.sort[,4] , y= data.sort[,5]))+
  geom_point()+
  geom_text(aes(label=data.sort[,6]),color=data.sort[,6],position = "identity",hjust=0, vjust=0)+
  labs(title="Dense 2 with 50 epochs")+xlab("D1")+ylab("D2")+
  theme_bw()


data.k<-kmeans(data.sort.filter[,c(4:5)],center=10)

data.number.f= data.sort.filter[,c(4,5)]
#data.num.dist<-dist(data.number.f)
#data.h<-hclust(dist(data.sort.filter[,c(4:5)]),method = "complete")

data.h<-hclust(dist(data.sort.filter[,c(4:5)]),method = "average")
plot(data.h)
cut.s.10 <- cutree(data.h, k=45)
data.number.f= data.sort.filter[,c(4,5)]
labels.f=data.sort.filter[,6]
ggplot(data = data.number.f)+
  geom_point(aes(x=data.number.f[,1] , y= data.number.f[,2]),color=cut.s.10,position = "identity",labels = labels.f)+
  labs(title="Dense 2 with 50 epochs")+xlab("D1")+ylab("D2")+
  theme_bw()

ggplot(data = data.number.f,aes(x=data.number.f[,1] , y= data.number.f[,2]))+
  geom_point()+
  geom_text(aes(label=labels.f),color=cut.s.10,position = "identity",hjust=0, vjust=0)+
  labs(title="Dense 2 with 50 epochs")+xlab("D1")+ylab("D2")+
  theme_bw()



data.cov=cov(as.matrix(data.sort.number))
dens.pca<-princomp(covmat=data.cov)
dens.pca<-princomp(as.matrix(data.sort.number),cor = T )



score<-as.data.frame(dens.pca$scores)
score<-cbind(score,labels)
apply(score,2,var)
summary(dens.pca)
loadings(dens.pca)
plot(dens.pca$scores[,1],dens.pca$scores[,2],type='n',ylab="PC2",xlab="PC1")
text(dens.pca$scores[,1],dens.pca$scores[,2],labels = labels)
#title(main="Dense Layer 10 PCA")
title(main="Dense Layer 128 PCA")
library(ggplot2)

ggplot(data = data.sort)+
  geom_point(aes(x=data.sort[,4] , y= data.sort[,5]),color=data.sort[,6],position = "identity")+
  labs(title="Dense 2 with 50 epochs")+xlab("D1")+ylab("D2")+
  theme_bw()



ggplot(score)+
  geom_point(aes(x=score[,1] , y= score[,2]),color=score[,3],position = "identity")+labs(title="Dense 2 with PC ")+xlab("PC1")+ylab("PC2")+theme_bw()





ggplot(data = data.sort.filter,aes(x=data.sort.filter[,4] , y= data.sort.filter[,5]))+
  geom_point()+
  geom_text(aes(label=data.sort.filter[,6]),color=data.sort.filter[,6],position = "identity",hjust=0, vjust=0)+
  labs(title="Dense 2 with 50 epochs filtered")+xlab("D1")+ylab("D2")+
  theme_bw()


#data.num.matr<-as.matrix(data.sort.filter[,4:5])
data.num.matr<-as.matrix(data.sort.filter[,-3])
data.filter.dist<-dist(data.num.matr)
#data.filter.dist<-cosine(t(data.num.matr))
data.mds<-cmdscale(data.filter.dist,k=2,eig=T)
summary(data.mds)

mdsscore<-as.data.frame(data.mds$points)

mdsscore<-cbind(mdsscore,data.sort.filter[,3])
ggplot(data = mdsscore,aes(x=mdsscore[,1] , y= mdsscore[,2]))+
  geom_point()+geom_text(aes(label=mdsscore[,3]),hjust=0, vjust=0)+
  theme_bw()
ggplot(data = mdsscore)+
  geom_point(aes(x=mdsscore[,1] , y= mdsscore[,2]),color=mdsscore[,3],position = "identity")+
  theme_bw()


ggplot(data = data.sort.filter)+
  geom_point(aes(x=data.sort.filter[,1] , y= data.sort.filter[,2]),color=data.sort.filter[,3],position = "identity")+
  theme_bw()

table(data.sort.filter$label)
data.sort.f.number<-data.sort.filter[,-c(1:3,132)]
labels.f<-data.sort.filter$label
dens.f.pca<-princomp(as.matrix(data.sort.f.number),cor = T )
score.s<-as.data.frame(dens.f.pca$scores)
score.s<-cbind(score.s,labels.f)
apply(score.s,2,var)
summary(dens.f.pca)
loadings(dens.f.pca)
plot(dens.f.pca$scores[,1],dens.f.pca$scores[,2],type='n')
text(dens.f.pca$scores[,1],dens.f.pca$scores[,2],labels = cut.s.5)

highlight.gene <- "83"

set.seed(23456)

score.s$highlight <- ifelse(score.s$labels.f == highlight.gene, "highlight", "normal")
textdf <- score.s[score.s$labels.f == highlight.gene, ]
mycolours <- c("highlight" = "red", "normal" = "grey50")


ggplot(score.s, aes(x=score.s[,1] , y= score.s[,2],color=highlight,label=score.s[,129],size=0.2))+
  geom_point() +geom_text(aes(label=score.s[,129],size=10),hjust=0, vjust=0)+
scale_color_manual("Status", values = mycolours)+labs(title="Dense Layer 128 extract version")+xlab("PC1")+ylab("PC2")+theme_bw()



pca<-score.s[,c(1:2,129)]

write.csv(pca,file ="DensePCA.csv",row.names = T)
