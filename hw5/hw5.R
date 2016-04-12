# hw5-sample.R - sample code for hw5
#
# @author: Yu-Ru Lin
# @date: 2015-03-17
# modified by Xynoci

# import packages
library(plyr)
library(ggplot2)
library(tm)
library(SnowballC)
library(lsa)

# load data
fromPath <- "http://www.yurulin.com/class/spring2016_datamining/data/hw5_1_dataset/r52.csv"
r52 <- read.csv(fromPath)
dim(r52)
r52[1:2,]

## Plot the histogram of number of documents per topic. Find and list the four most popular topics
## in terms of number of documents.

# plot the histrgram
topic<-as.data.frame(summary(r52$Topic))
topic<-cbind(rownames(topic),topic)
colnames(topic)<-c("Topics","Count")

ggplot(topic, aes(x = reorder(Topics, -Count),y=Count)) + 
  geom_histogram(stat = "identity")+
  labs(list(title="Histogtam of Topics",x="Topics",y="Count"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# find the most popular four topics
topics.pop <- sort(table(sample(r52$Topic)), decreasing = T)[1:4]
select.topics <- names(topics.pop)
topics.pop 

## Extract contents in these top 4 topics as your corpus. Run pre-processing on this corpus and use
## terms that appear at least four times in the corpus to create a term-document matrix. Use the
## term-document matrix to generate an MDS plot where each node represents a document with color
## indicating its topic.
doc.idx = which(r52$Topic %in% select.topics)
dataset = r52[doc.idx, ]

# create a corpus
corpus = Corpus(VectorSource(dataset$Content))
corpus

# convert to lower case, remove punctuation, remove numbers, remove stop words
# http://stackoverflow.com/questions/24771165/r-project-no-applicable-method-for-meta-applied-to-an-object-of-class-charact
corpus <- tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus = tm_map(corpus, stemDocument, language = "english")
corpus = tm_map(corpus, stripWhitespace)
inspect(corpus[1:10])

tdm<-TermDocumentMatrix(corpus)
td.mat = as.matrix(tdm)
dim(td.mat)

doc.idx.4<-which(rownames(td.mat) %in% findFreqTerms(tdm, 4))
td.mat.4<-td.mat[doc.idx.4,]

system.time({dist.mat.4<-dist(t(td.mat.4))})

doc.mds.4<-cmdscale(dist.mat.4, k=2)
data = data.frame(x = doc.mds.4[, 1], y = doc.mds.4[, 2], topic = dataset$Topic)
ggplot(data, aes(x = x, y = y, color = topic)) +
  geom_point()+ 
  ggtitle("MDS plot")

## Apply TFIDF weighting, latent semantic analysis (LSA) and non-negative matrix factorization (NMF) on the
## term-document matrix. Generate MDS plots corresponding to these matrices (TFIDF weighted matrix, LSA 
## approximated matrix, and NMF approximated matrix).

library(NMF)
Sys.setenv(NOAWT= "true") ## work around for the stemmer issue
# library(tm)
# library(lsa)
# library(ggplot2)
# dist.mat = dist(t(as.matrix(td.mat)))
system.time({})

do.svd <- function(data.mat, k) {
  S<-svd(as.matrix(data.mat), nu=k, nv=k)
  u<-S$u
  s<-S$d
  v<-S$v
  td.mat.svd <- u %*% diag(s[1:k]) %*% t(v)
  td.mat.svd
}

do.plot<-function(dist.mat, k, dataset, title){
  doc.mds<-cmdscale(dist.mat, k)
  data<-data.frame(x=doc.mds[,1], y=doc.mds[,2], topic=dataset$Topic)
  ggplot(data, aes(x = x, y = y, color = topic)) +
    geom_point()+ 
    ggtitle(title)
}

## tf-idf weighting
td.mat.w.tf<-lw_tf(td.mat.4)*gw_idf(td.mat.4)

## LSA
lsa.space<-lsa(td.mat.w.tf, dims=3)

## NMF
set.seed(1)
res<-nmf(td.mat, 3, "lee")
V.hat<-fitted(res)
dim(V.hat)

## distances
system.time({
  dist.mat.tf<-dist(t(as.matrix(td.mat.w.tf)))
  dist.mat.lsa<-dist(t(as.textmatrix(lsa.space)))
  dist.mat.nmf<-dist(t(as.matrix(V.hat)))
})

## plotting
do.plot(dist.mat.tf, 2, dataset, "MDS plot - tfidf")
do.plot(dist.mat.lsa, 2, dataset, "MDS plot - lsa")
do.plot(dist.mat.nmf, 2, dataset, "MDS plot - nmf")


## TASK 2
library(igraph)
library(plyr)

from = "http://www.yurulin.com/class/spring2016_datamining/data/hw5_2_dataset/user_artists.csv"
dataset = read.csv(from, sep = ",", header = T)
dim(dataset)

# preview the dataset
dataset[1:5,]
#   userID artistID weight
# 1      2       51  13883
# 2      2       52  11690
# 3      2       53  11351
# 4      2       54  10300
# 5      2       55   8983
unique(as.factor(dataset[,1])) # number from 2 to 210, 1892 levels
unique(as.factor(dataset[,2])) # number from 1 to 18745, 17632 levels

# extract top artists
artist.fac.sum<-aggregate(dataset, by=list(dataset$artistID), FUN=sum)
artist.1500<-subset(artist.fac.sum, weight>=1500) 
artist.1500.sorted<-artist.1500[order(-artist.1500$weight),]
top.30<-artist.1500.sorted[1:30,] # top 30 most frequently listened
top.ids<-top.30$Group.1 # id of top 30

# generate the network
top<-subset(dataset, dataset$artistID %in% top.ids) # extract user-artist pairs [1] 9692    3
top[1:10,]

# map artistID with names
from = "http://www.yurulin.com/class/spring2016_datamining/data/hw5_2_dataset/artists.csv"
mapper = read.csv(from, sep = ",", header = T)
dim(mapper)
mapper[1:10,]
names<-subset(mapper, mapper$id %in% top.ids)
y<-mapvalues(top$artistID, from=as.vector(names$id), to=as.vector(names$name))
top.named<-data.frame(userID=top$userID, artist=y, weight=top$weight) # [1] 9692    3
y1<-mapvalues(top.ids, from=as.vector(names$id), to=as.vector(names$name))
top.30.named<-cbind(name=y1, top.30) # Group.1, weight, name [1] 30  5

user.artist<-graph.data.frame(top.named, directed = T)
mat <- as.matrix(get.adjacency(user.artist))
m2 <- t(mat) %*% mat
m2.red <- m2[(nrow(m2)-29):nrow(m2),(ncol(m2)-29):ncol(m2)] # reduced a-a matrix
dim(m2.red)

m2.replica<-m2.red
m2.replica[which(m2.replica<5)]=0 # filtered a-a matrix
g.artist = graph.adjacency(m2.replica, weighted = T, mode = "undirected", diag = F)
# reorder data.frame according to the order of nodes
top.30.to.g <- top.30.named[match(V(g.artist)$name, top.30.named$name),]
# assign size according to artist's weight
V(g.artist)$size<-as.numeric(top.30.to.g$weight/100000)
set.seed(1)
plot.igraph(g.artist, layout = layout.fruchterman.reingold, vertex.label = V(g.artist)$name)
plot.igraph(g.artist, layout = layout.fruchterman.reingold, vertex.label.cex = 0.7)
mtext("Top 30 artists")

top.10 <- top.30.named[order(-top.30.named$weight),]
top.10 <- data.frame(name=top.10$name, weight=top.10$weight)
top.10

## get modularity-based community
fc = fastgreedy.community(g.artist)
modularity(fc)
membership(fc)
set.seed(1)
plot(fc, g.artist, main = "modularity community", layout = layout.fruchterman.reingold, vertex.label.cex = 0.7)
dendPlot(fc)
mtext("Top 30 artists - dendPlot")

c.deg = degree(g.artist)
c.top = order(c.deg, decreasing = T)[1:5]  ## the top-5 nodes with highest degrees
V(g.artist)$size = abs(c.deg)/2
V(g.artist)$color = "#3498DB"
V(g.artist)[c.top]$color = "#E74C3C"
V(g.artist)[-c.top]$label.color = "#225378"
V(g.artist)[c.top]$label.color = "#8A0917"
V(g.artist)$label.cex = 0.7
V(g.artist)[c.top]$label.cex = 0.8
E(g.artist)$color = "gray"  ## highlight the top-5 nodes
set.seed(1)
plot(g.artist, layout = layout.lgl)
title("degree centrality")

clo = closeness(g.artist)
clo
top = order(clo, decreasing = T)[1:5]
## size node by closeness
V(g.artist)$size = abs(clo) * 1e+04
V(g.artist)$color = "#3498DB"
V(g.artist)[top]$color = "#E74C3C"
V(g.artist)[-top]$label.color = "#225378"
V(g.artist)[top]$label.color = "#8A0917"
V(g.artist)$label.cex = 0.7
V(g.artist)[top]$label.cex = 0.8
E(g.artist)$color = "gray"  ## highlight the top-5 nodes
set.seed(1)
plot(g.artist, layout = layout.lgl)
title("closeness")

bet = betweenness(g.artist)
bet
top = order(bet, decreasing = T)[1:5]
## size node by betweenness
V(g.artist)$size = abs(bet)/5
V(g.artist)$color = "#3498DB"
V(g.artist)[top]$color = "#E74C3C"
V(g.artist)[-top]$label.color = "#225378"
V(g.artist)[top]$label.color = "#8A0917"
V(g.artist)$label.cex = 0.7
V(g.artist)[top]$label.cex = 0.8
E(g.artist)$color = "gray"  ## highlight the top-5 nodes
set.seed(1)
plot(g.artist, layout = layout.lgl)
title("betweenness")

# page rank
pg = page.rank(g.artist)$vector
pg
top = order(pg,decreasing=T)[1:5]
V(g.artist)$size = abs(pg)*300
V(g.artist)$color = "#3498DB"
V(g.artist)[top]$color = "#E74C3C"
V(g.artist)[-top]$label.color = "#225378"
V(g.artist)[top]$label.color = "#8A0917"
V(g.artist)$label.cex = 0.7
V(g.artist)[top]$label.cex = 0.8
E(g.artist)$color = "gray"  ## highlight the top-5 nodes
set.seed(1)
plot(g.artist, layout = layout.lgl)
title("PageRank")

# Recommender system
fromPath <- "http://www.yurulin.com/class/spring2016_datamining/data/hw5_3_dataset/movies.csv"
fromPath.user <- "http://www.yurulin.com/class/spring2016_datamining/data/hw5_3_dataset/user_ratedmovies.csv"
movies <- read.csv(fromPath, sep = ",", header = T)
rated <- read.csv(fromPath.user, sep=",", header =T)
dim(movies) # [1] 10197    23
dim(rated) # [1] 855598      9
movies[1:3,]
rated[1:3,]

############### FORGET ABOUT THESE LINES BELOW ###############
unique(as.factor(movies$id)) # 10197 from 1 to 65133
unique(as.factor(rated$userID)) # 2113 from 75 to 71534
unique(as.factor(rated$movieID)) # 10109 from 1 to 65133
rt.count<-sort(table(rated$movieID), decreasing = T)
rt.count.50<-rt.count[rt.count>=50]
dim(rt.count.50) # 3261
movie.ids<-unique(as.factor(rownames(rt.count.50)))
length(movie.ids) # 3261
mapper<-subset(movies, movies$id %in% movie.ids)
dim(mapper) # [1] 3261   23
usr.count<-sort(table(rated$userID), decreasing = T)
usr.count.10<-usr.count[usr.count>=10]
usr.ids<-unique(as.factor(rownames(usr.count.10)))
length(usr.ids) # 2113

r1<-subset(rated, movieID %in% movie.ids) # [1] 756594      9
r2<-subset(r1, userID %in% usr.ids) # [1] 756594      9

titles<-mapvalues(r2$movieID, from=as.vector(mapper$id), to=as.vector(mapper$title))
rating.table<-data.frame(userID=r2$userID, movieID=r2$movieID, title=titles, rating=r2$rating) # [1] 756594      4

movie.set<-data.frame(from=rating.table$userID, to=rating.table$title, weight=rating.table$rating)
g<-graph.data.frame(movie.set)
mat<-get.adjacency(g)
mat.w<-get.adjacency(g, attr="rating")
mat.w[1:2113,(ncol(mat.w)-3260):ncol(mat.w)]
dim(mat.w)
############### FORGET ABOUT THESE LINES ABOVE ###############

d4 = data.frame(from = rated$userID, to = rated$movieID, weight = rated$rating)
g4 = graph.data.frame(d4)
mat4 = get.adjacency(g4)
mat.w4 = get.adjacency(g4, attr = "weight")
movie.idx = which(colSums(as.matrix(mat4))>=50)
user.idx = which(rowSums(as.matrix(mat4))>=10)
rmat = mat.w4[user.idx, movie.idx]
dim(rmat) # [1] 2113 3261

## Random, Popular, UBCF, IBCF;
## evaluate by k-fold k=5
## performance table MAE MSE RMSE
library(recommenderlab)
m = as.matrix(rmat)
r = as(m, "realRatingMatrix")
dim(r)
# as(r, "list")
# head(as(r,""data.frame))
rowCounts(r[1,])
rowMeans(r[1,])
as(r[1,],"list")
## generate a histogram to show the distribution of the ratings
hist(getRatings(r), breaks=10)
## the distribution after normalization 
hist(getRatings(normalize(r)), breaks=10)

## create a 5-fold cross validation scheme with the the Given-5 protocol, 
## i.e., for the test users all but three randomly selected items are withheld for evaluation.
scheme <- evaluationScheme(r, method="cross", k=5, given=5, goodRating=5)
scheme
rr<-Recommender(getData(scheme, "train"), method="RANDOM")
prr<-predict(rr, getData(scheme, "known"), type="ratings")
e.prr<-calcPredictionAccuracy(prr, getData(scheme, "unknown"))

rp<-Recommender(getData(scheme, "train"), method="POPULAR")
prp<-predict(rp, getData(scheme, "known"), type="ratings")
e.prp<-calcPredictionAccuracy(prp, getData(scheme, "unknown"))
# names(getModel(rp))
# getModel(rp)$topN
ucf<-Recommender(getData(scheme, "train"), method="UBCF")
pucf<-predict(ucf, getData(scheme, "known"), type="ratings")
e.ucf<-calcPredictionAccuracy(pucf, getData(scheme, "unknown"))

icf<-Recommender(getData(scheme, "train"), method="IBCF")
picf<-predict(icf, getData(scheme, "known"), type="ratings")
e.icf<-calcPredictionAccuracy(picf, getData(scheme, "unknown"))

col.name<-c("Random", "Popular", "UBCF", "IBCF")
result<-cbind(e.prr, e.prp, e.ucf, e.icf)
colnames(result)<-col.name
result
