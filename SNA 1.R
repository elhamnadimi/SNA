install_github("DougLuke/UserNetR")
install.packages("UserNetR")
install.packages("intergraph")
#install.packages("devtools")



library(statnet)
library(UserNetR)
library(devtools)
library(UserNetR)
library(igraph)
library(intergraph)

data("Moreno")
gender <- Moreno %v% "gender"
list.vertex.attributes(Moreno)
plot(Moreno, vertex.col = gender + 2, vertex.cex = 1.2)
network.size(Moreno)
summary(Moreno,print.adj=FALSE)
gden(Moreno)
components(Moreno)

#given its size and degree of interconnect- edness.
#The diameter of a network is a useful measure of this compactness

#A typical app- roach when there are multiple components
#is to examine the diameter of the largest component in the network
lgc <- component.largest(Moreno,result="graph")
gd <- geodist(lgc)
max(gd$gdist)

#Creating a Network Object in statnet
netmat1 <- rbind(c(0,1,1,0,0), c(0,0,1,1,0), c(0,1,0,0,0), c(0,0,0,0,0), c(0,0,1,0,0))
#The name of the rbind R function stands for row-bind.
rownames(netmat1) <- c("A","B","C","D","E")
colnames(netmat1) <- c("A","B","C","D","E")
net1 <- network(netmat1,matrix.type="adjacency")
class(net1)
summary(net1)
gplot(net1, vertex.col = 2, displaylabels = TRUE)



netmat2 <- rbind(c(1,2), c(1,3), c(2,3), c(2,4), c(3,2), c(5,3))
net2 <- network(netmat2,matrix.type="edgelist")
network.vertex.names(net2) <- c("A","B","C","D","E")
summary(net2)
gplot(net2, vertex.col = 2, displaylabels = TRUE)


as.sociomatrix(net1)
class(as.sociomatrix(net1))

all(as.matrix(net1) == as.sociomatrix(net1))
as.matrix(net1,matrix.type = "edgelist")


# we use two different methods to set a pair of 
#node att- ributes (called vertex attributes by statnet).
set.vertex.attribute(net1, "gender", c("F", "F", "M", "F", "M"))
net1 %v% "alldeg" <- degree(net1)
list.vertex.attributes(net1)
get.vertex.attribute(net1, "gender")

#Tie Attributes
list.edge.attributes(net1)

set.edge.attribute(net1,"rndval", runif(network.size(net1),0,1))
list.edge.attributes(net1)
summary(net1 %e% "rndval")
summary(get.edge.attribute(net1,"rndval"))



#A valued network is one where the network tie has some numeric value. 
#The key here are the ignore.eval and names.eval options.
#These two options, as set here, tell the network function
#to evaluate the actual values in the sociomatrix

netval1 <- rbind(c(0,2,3,0,0), c(0,0,3,1,0), c(0,1,0,0,0),
                 c(0,0,0,0,0), c(0,0,2,0,0))
netval1 <- network(netval1,matrix.type="adjacency",
                   ignore.eval=FALSE,names.eval="like") 
network.vertex.names(netval1) <- c("A","B","C","D","E")
list.edge.attributes(netval1)
get.edge.attribute(netval1, "like")
as.sociomatrix(netval1,"like")
as.sociomatrix(netval1)




#Creating a Network Object in igraph and
library(igraph)
inet1 <- graph.adjacency(netmat1)
class(inet1)
summary(inet1)
str(inet1)


inet2 <- graph.edgelist(netmat2)
summary(inet2)
V(inet2)$name <- c("A","B","C","D","E")
E(inet2)$val <- c(1:6)
summary(inet2)
str(inet2)



#Going Back and Forth Between statnet and igraph
library(intergraph)
class(net1)
net1igraph <- asIgraph(net1)
class(net1igraph)
str(net1igraph)


#Importing Network Data


detach("package:igraph", unload=TRUE)
netmat3 <- rbind(c("A","B"), c("A","C"), c("B","C"),c("B","D"), c("C","B"), c("E","C"))
net.df <- data.frame(netmat3)
net.df

write.csv(net.df, file = "MyData.csv", row.names = FALSE)
net.edge <- read.csv(file="MyData.csv")
net_import <- network(net.edge,matrix.type="edgelist")
summary(net_import)
gden(net_import)




#Filtering Based on Node Values
n1F <- get.inducedSubgraph(net1,
                           which(net1 %v% "gender" == "F"))


n1F
gplot(n1F,displaylabels=TRUE)
deg <- net1 %v% "alldeg"
n2 <- net1 %s% which(deg > 1)
gplot(n2,displaylabels=TRUE)

#Removing Isolates

data(ICTS_G10)
gden(ICTS_G10)
length(isolates(ICTS_G10))

n3 <- ICTS_G10
delete.vertices(n3,isolates(n3))
gden(n3)
length(isolates(n3))

#Filtering Based on Edge Values
data(DHHS)
d <- DHHS
d
gden(d)
op <- par(mar = rep(0, 4))
gplot(d,gmode="graph",edge.lwd=d %e% 'collab',
      edge.col="grey50",vertex.col="lightblue",
      vertex.cex=1.0,vertex.sides=10) 
par(op)


as.sociomatrix(d)[1:6,1:6]
#First, we examine the network ties for the first six members of the network.
#we determine where the collaboration 
#values are stored, and then use that
#to view the tie values for the same set of six actors.

list.edge.attributes(d)
as.sociomatrix(d,attrname="collab")[1:6,1:6]

#The summary of the network object tells us 
#that there are 447 ties in the DHHS network.
#We can easily see the distribution of tie values.
table(d %e%"collab")
d.val <- as.sociomatrix(d,attrname="collab") 
d.val[d.val < 3] <- 0
d.filt <- as.network(d.val, directed=FALSE,
                     matrix.type="a",ignore.eval=FALSE, names.eval="collab")
summary(d.filt,print.adj=FALSE)

gden(d.filt)
op <- par(mar = rep(0, 4)) 
gplot(d.filt,gmode="graph",displaylabels=TRUE,
                                 vertex.col="lightblue",vertex.cex=1.3, label.cex=0.4,label.pos=5, displayisolates=FALSE)
par(op)




op <- par(mar = rep(0, 4))
d.val <- as.sociomatrix(d,attrname="collab") 
gplot(d.val,gmode="graph",thresh=2,
      vertex.col="lightblue",vertex.cex=1.3, label.cex=0.4,label.pos=5, displayisolates=FALSE)
par(op)



# Transforming a Directed Network to a Non-directed Network
#R makes it easy to transform a directed network into a non-directed network.
#To do this you can use the symmetrize() function
net1mat <- symmetrize(net1,rule="weak")
net1mat

net1symm <- network(net1mat,matrix.type="adjacency")
network.vertex.names(net1symm) <- c("A","B","C","D","E")
summary(net1symm)





#Visualization
data(Moreno)
op <- par(mar = rep(0, 4),mfrow=c(1,2)) 
plot(Moreno,mode="circle",vertex.cex=1.5)
plot(Moreno,mode="fruchtermanreingold",vertex.cex=1.5)
par(op)


op <- par(mar = c(0,0,4,0),mfrow=c(1,2))
gplot(Moreno,gmode="graph",mode="random",vertex.cex=1.5,main="Random layout") 
gplot(Moreno,gmode="graph",mode="fruchtermanreingold",
                                                                                          vertex.cex=1.5,main="Fruchterman-Reingold") par(op)


op <- par(mar=c(0,0,4,0),mfrow=c(2,3))
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='circle',main="circle") 
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='eigen',main="eigen") 
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='random',main="random")
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='spring',main="spring")
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='fruchtermanreingold',main='fruchtermanreingold')
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='kamadakawai',main='kamadakawai')
par(op)




mycoords1 <- gplot(Bali,gmode="graph", vertex.cex=1.5)

mycoords2 <- mycoords1

mycoords2[,2] <- mycoords1[,2]*1.5

mycoords1
op <- par(mar=c(4,3,4,3),mfrow=c(1,2))
gplot(Bali,gmode="graph",coord=mycoords1,vertex.cex=1.5,suppress.axes = FALSE, ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1), main="Original coordinates")
gplot(Bali,gmode="graph",coord=mycoords2, vertex.cex=1.5,suppress.axes = FALSE, ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1), main="Modified coordinates")




#Network Graph Layouts Using igraph

detach(package:statnet)


iBali <- asIgraph(Bali)

op <- par(mar=c(0,0,3,0),mfrow=c(1,3))
plot(iBali,layout=layout_in_circle,
     main="Circle")
plot(iBali,layout=layout_randomly,
     main="Random")
plot(iBali,layout=layout_randomly,
     main="Kamada-Kawai")
par(op)





#Effective Network Graphic Design

data(Bali)
gplot(Bali,vertex.col="slateblue2",gmode="graph")
col2rgb('slateblue2') 
gplot(Bali,vertex.col=rgb(122,103,238,maxColorValue=255),gmode="graph")
gplot(Bali,vertex.col="#7A67EE",gmode="graph")
summary(Bali)

#ff <- rgraph(5,tprob=0.5,mode="graph")
#ff

ndum <- rgraph(300,tprob=0.025,mode="graph")
ndum
op <- par(mar = c(0,0,2,0),mfrow=c(1,2))
gplot(ndum,gmode="graph",vertex.cex=1.5,
      vertex.col=rgb(0,0,139,maxColorValue=255),
      edge.col="grey80",edge.lwd=0.5, main="Fully opaque")
gplot(ndum,gmode="graph",vertex.cex=2,
      vertex.col=rgb(0,0,139,alpha=80,maxColorValue=255),
      edge.col="grey80",edge.lwd=0.5,main="Partly transparent")


rolelab <- get.vertex.attribute(iBali,"role")
Bali
op <- par(mar=c(0,0,0,0)) 
plot(Bali,usearrows=FALSE,vertex.cex=1.5,
     label=rolelab,displaylabels=T,vertex.col="role") 
par(op)
palette()




library(RColorBrewer)
display.brewer.pal(5, "Dark2")
my_pal <- brewer.pal(5,"Dark2")
rolecat <- as.factor(get.vertex.attribute(iBali,"role")) 
plot(iBali,vertex.cex=1.5,label=rolelab,
     displaylabels=T,vertex.col=my_pal[rolecat])




op <- par(mar=c(0,0,0,0))
sidenum <- 3:7 
plot(Bali,usearrows=FALSE,vertex.cex=4,
                    displaylabels=F,vertex.sides=sidenum[rolecat])
par(op)



op <- par(mar = c(0,0,2,0),mfrow=c(1,3))
plot(Bali,vertex.cex=0.5,main="Too small")
plot(Bali,vertex.cex=2,main="Just right")
plot(Bali,vertex.cex=6,main="Too large") 
par(op)

deg <- degree(iBali) 
deg

cls <- closeness(iBali)
cls

bet <- betweenness(iBali)
bet



op <- par(mar = c(0,0,2,1),mfrow=c(1,2)) 

plot(Bali,usearrows=T,vertex.cex=deg,main="Raw") 
plot(Bali,usearrows=FALSE,vertex.cex=log(deg),main="Adjusted")
par(op)


op <- par(mar = c(0,0,2,1),mfrow=c(1,2)) 
plot(Bali,usearrows=T,vertex.cex=cls,main="Raw")
plot(Bali,usearrows=FALSE,vertex.cex=4*cls,main="Adjusted")
par(op)


op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
plot(Bali,usearrows=T,vertex.cex=bet,main="Raw") 
plot(Bali,usearrows=FALSE,vertex.cex=sqrt(bet+1),main="Adjusted") 




rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl}
plot(Bali,vertex.cex=rescale(deg,1,6),
     main="Adjusted node sizes with rescale function.")



#Node Label

get.vertex.attribute(iBali,"vertex.names")
op <- par(mar = c(0,0,0,0)) 
plot(Bali,displaylabels=TRUE,label.cex=0.8,
     pad=0.4,label.col="darkblue") 
par(op)



rolelab <- get.vertex.attribute(iBali,"role")
rolelab
plot(Bali,usearrows=FALSE,label=rolelab,displaylabels=T,label.col="darkblue")

#Edge Width
op <- par(mar = c(0,0,0,0))
IClevel <- Bali %e% "IC" 
IClevel
plot(Bali,vertex.cex=1.5,edge.lwd=1.5*IClevel) 
par(op)


#Edge Color
n_edge <- network.edgecount(Bali)
n_edge
edge_cat <- sample(1:3,n_edge,replace=T)
linecol_pal <- c("blue","red","green")

plot(Bali,vertex.cex=1.5,vertex.col="grey25",
     edge.col=linecol_pal[edge_cat],edge.lwd=2)



#Legends
my_pal <- brewer.pal(5,"Dark2")
rolecat <- as.factor(get.vertex.attribute(iBali,"role")) 
plot(Bali,vertex.cex=rescale(deg,1,5),
     vertex.col=my_pal[rolecat])
legend("bottomleft",legend=c("BM","CT","OA","SB","TL"),
       col=my_pal,pch=19,pt.cex=1.5,bty="n", title="Terrorist Role")

       
#Description and Analysis
#Three Common Measures of Centrality
data(DHHS)
DHHS
#iDHHS<-asIgraph(DHHS)
df.prom <- data.frame(deg = degree(DHHS),
                      cls = closeness(DHHS),
                      btw = betweenness(DHHS),
                      evc = evcent(DHHS),
                      inf = infocent(DHHS), 
                      flb = flowbet(DHHS))


dum1 <- rbind(c(1,2),c(1,3),c(1,4),c(1,5))
star_net <- network(dum1,directed=FALSE)
dum2 <- rbind(c(1,2),c(2,3),c(3,4),c(4,5),c(5,1))
circle_net <- network(dum2,directed=FALSE)
par(mar=c(4,4,.1,.1))
my_pal <- brewer.pal(5,"Set2")

gplot(star_net,usearrows=FALSE,displaylabels=FALSE,
      vertex.cex=2,
      vertex.col=my_pal[1], edge.lwd=0,edge.col="grey50",xlab="Star Graph")

gplot(circle_net,usearrows=FALSE,displaylabels=FALSE, vertex.cex=2,
      vertex.col=my_pal[3], edge.lwd=0,edge.col="grey50",xlab="Circle Graph")

closeness(asIgraph(circle_net))




data(Bali)
str(degree(asIgraph(Bali)))
summary(degree(asIgraph(Bali)))


data(Bali)
my_pal <- brewer.pal(5,"Set2")  
rolecat <- Bali %v% "role"
gplot(Bali,usearrows=FALSE,displaylabels=TRUE,
      vertex.col=my_pal[as.factor(rolecat)], edge.lwd=0,edge.col="grey25")
legend("topright",legend=c("BM","CT","OA","SB", "TL"),col=my_pal,pch=19,pt.cex=2)


#Subgroup
#A clique is a maximally complete subgraph; that is,
#it is a subset of nodes that have all possible ties among them.
library(igraph)
clqexmp <- graph.formula(A:B:C:D--A:B:C:D,D-E,E-F-G-E)
clqexmp
clique.number(clqexmp)
cliques(clqexmp, min=3)
maximal.cliques(clqexmp,min=3)
largest.cliques(clqexmp)


#Coreness

Vname <- get.vertex.attribute(iDHHS,name='vertex.names', index=V(iDHHS))
Vname
V(iDHHS)$name <- Vname 
coreness <- graph.coreness(iDHHS)
V(iDHHS)$color <- coreness + 1
op <- par(mar = rep(0, 4))
plot(iDHHS,vertex.label.cex=0.6) 
par(op)


coreness <- graph.coreness(iDHHS)
table(coreness)

maxCoreness <- max(coreness) 
maxCoreness

colors <- rainbow(maxCoreness) 
op <- par(mar = rep(0, 4)) 
plot(iDHHS,vertex.label=coreness,vertex.color=colors[coreness]) 
par(op)
V(iDHHS)$name <- coreness
V(iDHHS)$color <- colors[coreness]



V(iDHHS)$name <- coreness 
V(iDHHS)$color <- colors[coreness]
iDHHS1_6 <- iDHHS
iDHHS2_6 <- induced.subgraph(iDHHS,vids=which(coreness > 1))
iDHHS3_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 2))
iDHHS4_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 3))

iDHHS5_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 4))
iDHHS6_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 5))
lay <- layout.fruchterman.reingold(iDHHS)
op <- par(mfrow=c(3,2),mar = c(3,0,2,0))
plot(iDHHS1_6,layout=lay,main="All k-cores")
plot(iDHHS2_6,layout=lay[which(coreness > 1),],
     main="k-cores 2-6")

plot(iDHHS3_6,layout=lay[which(coreness > 2),],
     main="k-cores 3-6")

plot(iDHHS4_6,layout=lay[which(coreness > 3),],
     main="k-cores 4-6")

plot(iDHHS5_6,layout=lay[which(coreness > 4),],
     main="k-cores 5-6")

plot(iDHHS6_6,layout=lay[which(coreness > 5),],
     main="k-cores 6-6")
par(op)


#Community Detection


#Modularity

g1 <- graph.formula(A-B-C-A,D-E-F-D,G-H-I-G,A-D-G-A) 
g1
V(g1)$grp_good <- c(1,1,1,2,2,2,3,3,3) 
V(g1)$grp_bad <- c(1,2,3,2,3,1,3,1,2)
g1
op <- par(mfrow=c(1,2))
plot(g1,vertex.color=(V(g1)$grp_good),vertex.size=30,main="Good Grouping")
plot(g1,vertex.color=(V(g1)$grp_bad),vertex.size=30,main="Bad Grouping")
par(op)
modularity(g1,V(g1)$grp_good)
modularity(g1,V(g1)$grp_bad)



data(DHHS)
iDHHS <- asIgraph(DHHS)
iDHHS
table(V(iDHHS)$agency)

V(iDHHS)[1:10]$agency

modularity(iDHHS,(V(iDHHS)$agency+1))



data(Moreno)
iMoreno <- asIgraph(Moreno)
table(V(iMoreno)$gender)
modularity(iMoreno,V(iMoreno)$gender)


data(Facebook)
levels(factor(V(Facebook)$group))
grp_num <- as.numeric(factor(V(Facebook)$group))
modularity(Facebook,grp_num)


#Community Detection Algorithms

modularity(g1,V(g1)$grp_good)
modularity(g1,V(g1)$grp_bad)

#The main reason that this chapter uses igraph is that it includes support for many
#if not most of the existing community detection approaches.

#The basic workflow for conducting community detection in igraph
#is to run one of the community detection functions on a network 
#and store the results in a communities class object
iMoreno
cw <- cluster_walktrap(iMoreno)
membership(cw)

#Modularity is fairly high, suggesting that the walktrap algorithm
#has done a good job at detecting subgroup structure.
#The membership function reveals that six dif- ferent subgroups
#have been identified

plot(cw, iMoreno)


#in this case, how does a walktrap solution 
#compare to the specific agency of the nodes in the DHHS network?


#we can position vertices in the same community group together and 
#make different communities stay further apart.


data(DHHS)
iDHHS<-asIgraph(DHHS)

iDHHS
cww <- cluster_walktrap(iDHHS)
modularity(cww)
membership(cww)
table(V(iDHHS)$agency,membership(cww))
class(cww)



rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}


modules <- decompose.graph(iDHHS)
out <- modules[order(sapply(modules, ecount), decreasing=T)]
length(out)
out


vertexes <- character()
data_frames <- list()
for(i in 1:length(out)) {
  vertexes[i] <- list(vertex.attributes(out[[i]])$vertex.names)
  data_frames[[i]] <- get.data.frame(out[[i]])
}


rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}


hh <- edge.betweenness.community(iDHHS, weights = NULL
                                         ,directed = FALSE,bridges = TRUE)
max(hh$membership)


#we can position vertices in the same community group together and 
#make different communities stay further apart.
LC_Grouped = iDHHS
E(LC_Grouped)$weight = 1
for(i in unique(membership(hh))) {
  GroupV = which(membership(hh) == i)
  LC_Grouped = add_edges(LC_Grouped, combn(GroupV, 2), attr=list(weight=6))
}

set.seed(1234)
LO = layout_with_fr(LC_Grouped)
colors <- rainbow(max(membership(LC.gn.comm)))
par(mar=c(0,0,2,0))
plot(hh, iDHHS, layout=LO,
     vertex.size = 6, 
     vertex.color=colors[membership(LC.gn.comm)], 
     vertex.label = NA, edge.width = 1,edge.color="gray60")

title(main="Giant component CIS Network.",sub="Girvan-Newman Algorithm", cex.main=2)









