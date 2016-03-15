# hw4sample.R - sample code for hw4
#
# @author: Yu-Ru Lin
# @date: 2015-02-18
# modified by Xynoci

######################### task 1 #########################
load.data.dji <- function() {
  data.url = 'http://www.yurulin.com/class/spring2016_datamining/data'
  dataset <- read.csv(sprintf("%s/stock_price.csv",data.url))  
  print(head(dataset))
  print(dim(dataset))
  n = nrow(dataset)
  t(dataset)
}
do.pca <- function(dataset, lbls, do.screeplot=F, do.scatter=F, do.biplot=F, do.loadingplot=F) {
  data.pca = prcomp(dataset, scale=TRUE, center=TRUE) 
  ## the loadings
  data.pca$rotation
  ## use 'predict' to project data onto the loadings
  data.pc = predict(data.pca)
  data.pc
  if (do.screeplot){
    plot(data.pca, main="")
    mtext(side=1,"screeplot for PCA on financial indicators", line=1, font=2)
  } 
  if (do.scatter) {
    plot(data.pc[,1:2], type="n")
    text(x=data.pc[,1], y=data.pc[,2], labels=lbls, cex=0.9)    
  }
  if (do.biplot) 
    biplot(data.pca)
  if (do.loadingplot) {
    plot(data.pca$rotation[,1],type='l')
    # plot(data.pc[,1],type='p')
    mtext(side=3,"Loadings of the first PC", line=1, font=2)
  }
  data.pc
}
do.mds <- function(dataset,lbls,do.scatter=T) {
  data.dist = dist(dataset)
  data.mds = cmdscale(data.dist)
  if (do.scatter) {
    plot(data.mds, type = 'n')
    text(data.mds,labels=lbls,cex=0.8)       
    mtext(side=3,"MDS map", line=1, font=2)
  }
  data.mds
}
do.kmeans <- function(dataset,lbls,k=3,do.scatter=F) {
  set.seed(123)
  data.clu = kmeans(dataset, centers=k, nstart=10)
  if (do.scatter) {
    plot(dataset,type='n')
    text(dataset,labels=lbls,col=rainbow(k)[data.clu$cluster])    
  }
  data.clu
}
do.hclust <- function(dataset,lbls,k=3,do.dendrogram=T,do.method='complete') {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method=do.method) ## change method to be single, complete, average, etc.
  if (do.dendrogram) {
    plot(hc)
  }
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
do.mds.plot <- function(data.mds, cluster){
  plot(data.mds, type="n")
  text(data.mds, labels, col=rainbow(6)[cluster])
}


######################### task2 #########################
rollcall.simplified <- function(df) {
  no.pres <- subset(df, state < 99)
  ## to group all Yea and Nay types together
  for(i in 10:ncol(no.pres)) {
    no.pres[,i] = ifelse(no.pres[,i] > 6, 0, no.pres[,i])
    no.pres[,i] = ifelse(no.pres[,i] > 0 & no.pres[,i] < 4, 1, no.pres[,i])
    no.pres[,i] = ifelse(no.pres[,i] > 1, -1, no.pres[,i])
  }
  
  return(as.matrix(no.pres[,10:ncol(no.pres)]))
}

library('foreign') ## for loading dta files using read.dta
library('ggplot2')
library(plyr) # for recoding data
theme_set( theme_bw( base_family="Helvetica")) 
# the font changign doesn't work for Mac
theme_update(plot.title = element_text( size=11,vjust=1,face='bold'),
             axis.title.x = element_text( size=12),
             axis.title.y = element_text( size=12,angle=90 ),
             axis.text.x = element_text( size=10),
             axis.text.y = element_text( size=10,hjust=1 ))

load.roll.call <- function(congr=11) { ## extract the 10th congress data by default
  data.url = 'http://www.yurulin.com/class/spring2016_datamining/data/roll_call'
  data.files = c("sen101kh.dta", "sen102kh.dta",
                 "sen103kh.dta", "sen104kh.dta",
                 "sen105kh.dta", "sen106kh.dta",
                 "sen107kh.dta", "sen108kh_7.dta",
                 "sen109kh.dta", "sen110kh_2008.dta",
                 "sen111kh.dta")
  dataset = read.dta(file.path(data.url, data.files[congr]), convert.factors = FALSE)
  dataset = subset(dataset, state < 99)
  print(dim(dataset))
  print(head(dataset[,1:12]))
  dataset
}

roll.call.mds <- function(dataset,do.scatter=T,do.scatter.ggplot=T,do.clust, do.method) {
  get.dist <- function(m) {
    dist(m %*% t(m))
  }
  
  data1 = rollcall.simplified(dataset)
  
  ## use either kmeans or hclust
  if (do.clust=='kmeans') { 
    clu = do.kmeans(data1,NULL,k=2)$cluster
  }
  else if (do.clust=='hclust') { 
    clu = do.hclust(data1,NULL,k=2, do.method = do.method)
  }
  else {}
  print(clu) 
  print(dim(data1))
  #print(head(data1[,1:12]))  
  data.dist = get.dist(data1)   #get distance matrix
  print(data.dist)
  
  lbls = dataset$name
  party = mapvalues(dataset$party,from=c(100, 200, 328),to=c("Dem", "Rep", "Ind") )
  data.mds = cmdscale(data.dist)
  if (do.scatter) {
    plot(data.mds, type = 'n')
    text(data.mds,labels=lbls)       
  }
  data2 = data.frame(x=data.mds[,1],y=data.mds[,2],name=lbls,party=party,clu=factor(clu))
  data3 = data.frame(x=data.mds[,1],y=data.mds[,2],name=lbls,party=party,clu=factor(dataset$party))
  if (do.scatter.ggplot) {
    p = ggplot(aes(x=x,y=y,shape=party,color=clu), data=data2) +
      geom_point(size=4,alpha=0.5) +
      geom_text(aes(x=x,y=y,shape=party,color=clu,label=name), size=3)
    print(p)
  }
  p = ggplot(aes(x=x,y=y,shape=party,color=party), data=data3) +
    geom_point(size=4,alpha=0.5) +
    geom_text(aes(x=x,y=y,shape=party,color=party,label=name), size=3)
  print(p)
  
  p<-cluster.purity(clu,party)
  e<-cluster.entropy(clu,party)
  diff<-data.frame(t(rbind(t(dataset$name), as.vector(dataset$party), as.vector(clu))))
  list(data.mds, diff,p,e)
}
cluster.purity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}
cluster.entropy <- function(clusters, classes) {
    en <- function(x) {
    s = sum(x)
    sum(sapply(x/s, function(p) {if (p) -p*log2(p) else 0} ) )
  }
  M = table(classes, clusters)
  m = apply(M, 2, en)
  c = colSums(M) / sum(M)
  sum(m*c)
}


if(1){
  dataset = load.data.dji()
  labels = rownames(dataset)
  data.pc = do.pca(dataset, labels, do.screeplot=T, do.scatter=T,do.loadingplot=T)
  data.stand = t(scale(t(dataset)))
  data.mds = do.mds(data.stand,labels)
  clu_k_3 = do.kmeans(data.pc, labels)$cluster
  clu_h_3_c = do.hclust(data.pc, labels, do.dendrogram = T, do.method='complete')
  clu_h_3_s = do.hclust(data.pc, labels, do.dendrogram = T, do.method='single')
  clu_h_3_a = do.hclust(data.pc, labels, do.dendrogram = T, do.method='average')
  clu_k_6 = do.kmeans(data.pc, labels, k=6)$cluster
  clu_h_6_c = do.hclust(data.pc, labels, k=6, do.dendrogram = T, do.method='complete')
  clu_h_6_s = do.hclust(data.pc, labels, k=6, do.dendrogram = T, do.method='single')
  clu_h_6_a = do.hclust(data.pc, labels, k=6, do.dendrogram = T, do.method='average')
  do.mds.plot(data.pc,clu_k_3)
  do.mds.plot(data.pc,clu_h_3_c)
  do.mds.plot(data.pc,clu_h_3_s)
  do.mds.plot(data.pc,clu_h_3_a)
  do.mds.plot(data.pc,clu_k_6)
  do.mds.plot(data.pc,clu_h_6_c)
  do.mds.plot(data.pc,clu_h_6_s)
  do.mds.plot(data.pc,clu_h_6_a)
}

if (1) { ## task2
  dataset = load.roll.call()
  k<-roll.call.mds(dataset,do.scatter=T,do.scatter.ggplot=T, do.clust='kmeans')
  ha<-roll.call.mds(dataset,do.scatter=T,do.scatter.ggplot=T, do.clust='hclust', do.method='average')
  hs<-roll.call.mds(dataset,do.scatter=T,do.scatter.ggplot=T, do.clust='hclust', do.method='single')
  hc<-roll.call.mds(dataset,do.scatter=T,do.scatter.ggplot=T, do.clust='hclust', do.method='complete')
  k.diff<-data.frame(k[2])
  ha.diff<-data.frame(ha[2])
  hc.diff<-data.frame(hc[2])
  hs.diff<-data.frame(hs[2])
  k.wrong.dem<-subset(k.diff, k.diff['X3']==2&k.diff['X2']==200)['X1']
  k.wrong.rep<-subset(k.diff, k.diff['X3']==1&k.diff['X2']==100)['X1']
  ha.wrong.dem<-subset(ha.diff, ha.diff['X3']==2&ha.diff['X2']==200)['X1']
  ha.wrong.rep<-subset(ha.diff, ha.diff['X3']==1&ha.diff['X2']==100)['X1']
  hc.wrong.dem<-subset(hc.diff, hc.diff['X3']==2&hc.diff['X2']==200)['X1']
  hc.wrong.rep<-subset(hc.diff, hc.diff['X3']==1&hc.diff['X2']==100)['X1']
  hs.wrong.dem<-subset(hs.diff, hs.diff['X3']==2&hs.diff['X2']==200)['X1']
  hs.wrong.rep<-subset(hs.diff, hs.diff['X3']==1&hs.diff['X2']==100)['X1']
  purity<-c(k[3],hs[3],hc[3],ha[3])
  entropy<-c(k[4],hs[4],hc[4],ha[4])
  column.name<-c("k-means","hcluster-single", "hcluster-complete", "hcluster-average")
  row.name<-c("Purity","Entropy")
  summary<-matrix(c(purity,entropy), nrow=2, ncol=4, byrow=TRUE, dimnames=list(row.name, column.name))
  summary
  
}
