library(ggnet)
library(network)
library(sna)
library(ggplot2)
#devtools::install_github("briatte/ggnet")
#library(ggnet)
#library("GGally", lib.loc="/ihome/jwoo/glasso/test")
#library(GGally)
#kohda<-as.matrix(read.table("AICPRECISION.txt")) # result with AIC
#t<-as.matrix(read.table("BICPRECISION.txt"))  # result with BIC
#cvval<- as.matrix(read.table("CVPRECISION.txt"))  # result with CV
#u<-as.matrix(read.table("originconx.txt"))     # original conx 

SIMUNETWORKS <- function(kohda, filename, u){

print(kohda)
print(u)

#print(kohda)
#print(t)
#print(u)


totest <- kohda +u 
print(kohda)
print(u)
print(totest)

for(j in 1:dim(totest)[1]){
   for(i in 1:dim(totest)[2]){
   	 if(totest[i,j]==1){
	 totest[i,j]<-0
	 }
	 else if (totest[i,j]==0){
	 totest[i,j]<-1	 
	 }
        }
}

kohda.net <- as.network.matrix(kohda, directed=F,names.eval='impact') #make network object

#kohda.net %v% "depth" = c(rep("shallow",2))#, rep("shallow",5), rep("mid",2), "shallow", "mid", rep("deep",2), rep("mid",4), 
#"shallow", rep("deep",2), "mid", rep("deep",2), "shallow", rep("mid",5), "shallow"

#set.edge.attribute(kohda.net, "lty", ifelse(kohda.net %e% "weights" > 1, "red", "green"))
edge_color = ifelse(kohda>0, "red","green")
print(edge_color)
ggnetplot<-
ggnet2(kohda.net, color = "phono", palette = "Set1", edge.color = c("color", "grey50"), label=T)
kohda.net%e%'impact'<-totest
jpeg(filename)
print(kohda.net%e%'impact')
print(ifelse(kohda.net%e%'impact'==2,'red','green'))
plot(kohda.net,edge.col=ifelse(kohda.net%e%'impact'==2,'red','green'),displayisolates=TRUE)
}

#SIMUNETWORKS(kohda, "AICPRECISION.txt", u)
#SIMUNETWORKS(t, "BICPRECISION.txt", u)
#SIMUNETWORKS(cvval, "CVPRECISION.txt", u)

#totest2 <- t +u
#for(jj in 1:dim(totest2)[1]){
#   for(ii in 1:dim(totest2)[2]){
#         if(totest2[ii,jj]==1){
#         totest2[ii,jj]<-0
#         }
#         else if (totest2[ii,jj]==0){
#         totest2[ii,jj]<-1
#         }
#        }
#}

#t.net <- as.network.matrix(t, directed=F,names.eval='impact') #make network object
#t.net%e%'impact'<-totest2
#jpeg('bicresult.jpg')
#plot(t.net,edge.col=ifelse(t.net%e%'impact'==2,'red','green'),displayisolates=TRUE)


#totest <- kohda +u
#for(j in 1:dim(totest)[1]){
#   for(i in 1:dim(totest)[2]){
#         if(totest[i,j]==1){
#         totest[i,j]<-0
#         }
#         else if (totest[i,j]==0){
#         totest[i,j]<-1
#         }
#        }
#}

#kohda.net <- as.network.matrix(kohda, directed=F,names.eval='impact') #make network object

#kohda.net %v% "depth" = c(rep("shallow",2))#, rep("shallow",5), rep("mid",2), "shallow", "mid", rep("deep",2), rep("mid",4),
#"shallow", rep("deep",2), "mid", rep("deep",2), "shallow", rep("mid",5), "shallow")

#set.edge.attribute(kohda.net, "lty", ifelse(kohda.net %e% "weights" > 1, "red", "green"))
#edge_color = ifelse(kohda>0, "red","green")
#print(edge_color)
#ggnetplot<-
#ggnet2(kohda.net, color = "phono", palette = "Set1", edge.color = c("color", "grey50"), label=T)
#kohda.net%e%'impact'<-totest
#jpeg('aicresult.jpg')
#plot(kohda.net,edge.col=ifelse(kohda.net%e%'impact'==2,'red','green'),displayisolates=TRUE)


#SIMUNETWORKS(kohda, t, cvval,u)

#print(kohda.net%e%'weights')
#print(kohda)
#ggnet.cichlids <- 
#  ggnet2(kohda.net, label=T, 
#       node.size = 8, node.color = "depth", 
#       color.palette = "Set2", color.legend = "Water Depth", 
#       edge.size = 1, edge.color = "green",
#       arrow.size = 9,  arrow.gap = 0.027, 
#       legend.size=20)  + 
#       guides(color=guide_legend(keyheight=0.5,default.unit="inch",
#                                 override.aes = list(size=6)))

#jpeg('aaa.jpg')
#plot(ggnet.cichlids)

#aaaaaaaaaaaaaaaaaaaaa

#bip = data.frame(event1 = c(1, 2, 1, 0),
#                 event2 = c(0, 0, 3, 0),
#                 event3 = c(1, 1, 0, 4),
#                 row.names = letters[1:4])

#bip = network(bip,
#              matrix.type = "bipartite",
#              ignore.eval = FALSE,
#              names.eval = "weights")

#set.edge.attribute(bip, "color", ifelse(bip %e% "weights" > 1, "black", "grey75"))
#ggnet2(bip, color = "mode", palette = col, edge.size = "weights", edge.color = "color")



#s<-as.matrix(read.table("simutesting.txt"))
#t<-as.matrix(read.table("originconx.txt"))
#u<-QUICFINDTPTN(s,t,nn=40,minIndex=0.5, minIndexBIC=0.6, valofmaxIter=10000, valoftol=0.0001)