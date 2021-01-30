install_github("DougLuke/UserNetR")
install.packages("UserNetR")
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
