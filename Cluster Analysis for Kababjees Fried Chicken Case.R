final<-read.csv(file.choose(),header=T)
str(final)
sum(is.na(final))
final$X<-NULL
final$rest_name<-NULL
str(final)
library(dplyr)
means<-apply(final, 2,mean)
std<-apply(final, 2,sd)
norm<- scale(final,center=means,scale=std)
nor<-data.frame(norm)
library(psych)
set.seed(123)
k.max<-10
wss<-rep(NA,k.max)
nClust <- list()
#
for(i in 1:k.max){
  
  kdata <- kmeans(nor,i)
  wss[i]<- kdata$tot.withinss
  nClust[[i]]<-kdata$size
}
plot(1:k.max,wss,type ="b",xlab ="Number of Clusters: K",ylab = "Total Within Clusters Sum of Squares")

set.seed(123)
kc2<- kmeans(nor[,c(6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22,23,24)],2)
kc2
set.seed(123)
kc4<- kmeans(nor[,c(6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22,23,24)],4)
kc4
set.seed(123)
kc5<- kmeans(nor[,c(6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22,23,24)],5)
kc5
install.packages("cluster")
library(cluster)
clusplot(final,kc4$cluster,
         colors=T,
         shade=T,
         labels=2,
         lines=0)
library(dplyr)
member<-final[,c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22,23,24)]
#List the primary target market Kababjees should focus on.
a<- member %>% 
  mutate(Cluster=kc4$cluster) %>% 
  group_by(Cluster) %>% 
  summarize_all("mean")
a


#q2
#Which media platforms should Kababjees use to target this TG
member1<-final[,c(1,2,3,4)]
b<- member1 %>% 
  mutate(Cluster=kc4$cluster) %>% 
  group_by(Cluster) %>% 
  summarise_all("mean")
b
