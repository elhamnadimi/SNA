install_github("DougLuke/UserNetR")
install.packages("UserNetR")
install.packages("intergraph")
#install.packages("devtools")



library(statnet)
library(UserNetR)
library(devtools)
library(UserNetR)

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
library(statnet)
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
