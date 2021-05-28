########################################
## Camila Barreto e Carla Pavone  ######
## Trabalho final: Macroecologia  ######
## Dados: Algas e Gramas          ######
## Analise de zonas biogeograficas######
## 27/10/2020                     ######
########################################


rm (list=ls()) # limpando todos os objetos do wokspace (Enrivonment)

####Definindo a proposta e o objetivo do estudo
#1a. definir as zonas biogeograficas para algas e gramas marinha
#1b. comparar as zonas definidas com variações nas condições abióticas e relacionar as principais variaveis de influencia para os dois grupos
#1c. (?) Relacionar também com impacto antrópico #anda estamos buscando uma fonte de dados que nos permita associar as medidas a coordenadas geográficas


###2. Dados de distribuicao das especies

library(vegan)

algas<-read.csv("algas_presabs.csv",header=T, dec=".", sep=";")
head(algas)

gramas<-read.csv("gramas_presabs.csv",header=T, dec=".", sep=";")
head(gramas)

a=decostand(algas,"pa")
a
b=decostand(gramas,"pa")
b

help(deconstand)
??decostand

###3. Metricas de distancia
#"We argue that for the purpose of biogeographical regionalizations 
#richness-independent turnover is more informative, and therefore suggest 
#the use of metrics that are least affected by the variation in richness." Kreft & Jetz 2010

library(betapart)
sp.jtu=beta.pair(algas,index.family="sorensen")$beta.sim

#simp=beta.pair(fami,index.family="jaccard")$beta.jtu

###testar cluster sem a priori sobre os grupos
#library(clustsig)
#res <- simprof(data=sp.jtu)
#pl.color <- simprof.plot(res)


###4. Geographical visualization of turnover


###5. Ordination  : time consuming!!!
library(vegan);library(recluster)
points<-metaMDS(a,distfun = betadiver,distance="sim", k=2,
                autotransform=F,trymax=5,parallel=8)$points
col<-recluster.col(points)
RGB=data.frame(BASIN=rownames(col),RGB=rgb(col[,3:5],maxColorValue=255))
recluster.plot.col(col,cex=5,cext=1)


###6. Clustering
#"However, it should be noted that the clustering approach establishes
#only statistical patterns and does not 
#allow direct evolutionary inference on areal relationships and historical patterns of vicariance and 
#dispersal." Kref & Jetz 2010
####
###TURNOVER
#Correlation among cophenetic distance and original distance
library(cluster)
sp.jtu.clust<- hclust(sp.jtu,method="average")#average
d2 <- cophenetic(sp.jtu.clust)
plot(as.vector(sp.jtu)~as.vector(d2))
cor(as.vector(sp.jtu),as.vector(d2))


#6a. Defining the number of clusters by Explained Dissimilarity Kref & Jetz 2010
library(recluster);library(ape)
#transform hclust to phylogenetic tree (ape)
tree=as.phylo(sp.jtu.clust)
#inspect explained diversity for different cuts of a tree
expl_div<-recluster.expl.diss(tree,sp.jtu,maxcl=100)
#expl_div
R2=expl_div$expl.div
clusters=expl_div$nclust#[-length(expl_div$nclust)]

library(segmented)
out.lm<-lm(R2~clusters)
o<-segmented(out.lm,seg.Z=~clusters,psi=list(clusters=c(15)),
             control=seg.control(display=FALSE))
slope(o)
o
k=round(o$psi[,2],0);k
plot(R2~clusters,main=paste("Clusters= ",k,sep=""));abline(h=0.9);abline(v=k,col="red",lwd=3);
plot(o,add=T)


#6b. Defining the number of clusters by KGS
library(maptree)
b <- kgs (cluster=sp.jtu.clust, diss=sp.jtu, maxclust=100)
k=as.numeric(names(b)[which(b==min(b))]);k
plot (names (b), b, xlab="# clusters", ylab="penalty",main=paste("Clusters= ",k,sep=""),cex=1);abline(v=k,lwd=3,col="red")

#pdf("resultado_sp_sim_kgs_Rich>4.pdf",width=40,height=20,pointsize=40)
layout(matrix(c(1,3,1,3,2,3,2,3),ncol=4))
id=sample(1:length(sp.jtu),length(sp.jtu)*0.1,replace=F)#10% dos pontos plotados
plot(sp.jtu[id]~d2[id],ylab="Bjtu (Turnover, Baselga-Jaccard)_30000sample",xlab="Cophenetic distance_30000sample",
     main=paste("Correlation = ",round(cor(sp.jtu, d2),2)),xlim=c(0,1),ylim=c(0,1));abline(a=0,b=1,lwd=3,col="red")
#plot(1,type="n",axis=F,labels=F,ylab="",xlab="",lty="none",box=F)
plot (names (b), b, xlab="# clusters", ylab="penalty",main=paste("Clusters= ",k,sep=""),cex=3);abline(v=k,lwd=3,col="red")

source("criando cluster coloridos.R")
A2Rplot(sp.jtu.clust,k=k,boxes=FALSE,col.up="gray50",type=c("rectangle"),
        lty.down=1,lwd.down=3,knot.pos=c("mean"),only.tree=TRUE)
#layout(matrix(c(1),ncol=1))

dev.off()


####MAP
# cut tree into N clusters
groups.sp.jtu <- cutree(sp.jtu.clust, k=k)
#groups.sp.jtu=expl_div$matrix[,12]

grupos=groups.sp.jtu

library(RColorBrewer)
#display.brewer.all()
nclr <- k
#display.brewer.pal(nclr,name="Dark2")#RdYlGn;#Greys
#display.brewer.pal(nclr,name="RdYlGn")#RdYlGn;#Greys #Set1
tons <- brewer.pal(nclr, "Set1")
#tons <- sample(rainbow(nclr))
tons

grupos.cor=groups.sp.jtu

for(i in 1:length(tons)){
    n=length(grupos.cor[grupos==i])
  grupos.cor[grupos==i]=rep(tons[i],n)
}
grupos.cor
gr.sp=data.frame(BASIN=names(grupos.cor),sp.JTU=grupos.cor)
head(gr.sp)



#plotando
library(maps)
mundo="/home/murilo/Desktop/GIS database/mundo/ne_10m_admin_0_countries.shp"
arq="/home/murilo/Desktop/GIS database/BassinsPablo_nova base_3000bacias_junho2013/Basin_3116.shp"

library(maptools);library(sp)
myshp<-readShapePoly(arq)
mundoshp<-readShapePoly(mundo)

head(gr.sp,10)
head(myshp@data,10)

##juncao com merge
myshp=merge(x = myshp,y = gr.sp,by="BASIN",sort=FALSE,nomatch=NA)
head(myshp@data)

##Juncao do DataFrame com o myshp@data
#myshp@data = data.frame(myshp@data, sp.JTU=gr.sp[match(myshp@data$BASIN, gr.sp$BASIN),"sp.JTU"])
#head(myshp@data)

table(myshp@data$sp.JTU)
#pdf("BiogeoZones_Bsim_sp>14.pdf",width=16,height=7.5)
#jpeg("BiogeoZones_Bsim_sp>4_k.jpeg",height=1800,width=3400,pointsize=17,res=300,quality=100)
par(mar=c(0.3,0.3,0.3,0.3))
#layout(matrix(c(1,1,2,1,1,3,1,1,4),ncol=3))
plot(mundoshp,col=gray(0.0),border=gray(0.0))
plot(myshp,col=as.vector(myshp@data$sp.JTU),border=as.vector(myshp@data$sp.JTU),add=T)
#spplot(myshp,"sp.JTU")
dev.off()

#plot(mundoshp,col=gray(0.95),lwd=0.01,xlim = c(-110, -100), ylim =c(10, 50),main="USA")#USA
#plot(myshp,col=as.vector(myshp@data$sp.JTU),add=T)
#
#plot(mundoshp,col=gray(0.95),lwd=0.01,xlim = c(20, 25), ylim = c(35, 41))#Grecia
#plot(myshp,col=as.vector(myshp@data$sp.JTU),add=T)
#
#plot(mundoshp,col=gray(0.95),lwd=0.01,xlim = c(35,60), ylim =c(-30,-10),main="Madagascar")#Madagascar
#plot(myshp,col=as.vector(myshp@data$sp.JTU),add=T)
#dev.off()
#
