set.seed(16)
SIMUGENERATE <- function(nn,samples,case, numhub, bighub, shlb, shub,bigwgt,disconnected){
ssigma <- matrix(0, nn,nn)
diag(ssigma) <- 1

covx<-matrix(0,nn,2)

for (i in 1:nn) {
covx[i,1]<-runif(1)
covx[i,2]<-runif(1)
}

dij<-matrix(0,nn,nn)
for (i in 1:nn) {
 for (j in 1:nn) {
 dij[i,j]<-sqrt((covx[i,1]-covx[j,1])^2+(covx[i,2]-covx[j,2])^2)
 }
}

################################### tested so far
################### modify the matrix
counts<-0

yyy<-bighub
conx <- matrix(0, nn, nn)
for (i in 1:numhub) {
tmp<-order(dij[i,1:nn],decreasing=F)[1:bighub]
conx[i,tmp]<-1
conx[tmp,i]<-1
}

#yyy<-smallhub

for (i in (numhub+1):nn) {
tmp<-order(dij[i,1:nn],decreasing=F)[shlb:shub]
conx[i,tmp]<-1
conx[tmp,i]<-1
}

for (xx in (nn-disconnected+1):nn){
  for (yy in 1:nn){
    conx[xx,yy]<-0
    conx[yy,xx]<-0
  }
  }

#print(apply(t(conx), 1, sum))
iphone<- apply(t(conx), 1, sum)
aaaaaa<-mean(iphone[1:5])
bbbbbb<-mean(iphone[6:24])
cccccc<-mean(iphone[6:40])
#write(c(aaaaaa,bbbbbb) ,'meannumedges.txt', append=T)
print(aaaaaa)
print(bbbbbb)
print(cccccc)
#sssssssssssssssssssssssstop to check apply result
#################
#################

diag(conx) <- 0
temp <- matrix(1, nn, nn)
 for (ii in 1:nn){
 for (iii in 1:ii){
      temp[iii, ii] <- 0
          }
}

premat<-matrix(runif(nn*nn, 0, 1), ncol=nn)
premat[temp==0] <- 0
diag(premat)<-1
premat[conx!=1] <- 0
premat <- premat + t(premat)
premat[premat > 0 & premat < 0.5] <- premat[premat > 0 & premat < 0.5] - 1
diag(premat) <- apply(abs(premat), 1, sum) + 0.5
premat1 <- sqrt(as.matrix(diag(premat))%*%t(as.matrix(diag(premat))))
premat <- premat/premat1
covmat<-solve(premat)
##find sqaure root of a matrix
eigencov<-eigen(covmat)
D<-matrix(0,nn,nn)
diag(D)<-sqrt(eigencov$values)
covmats<-eigencov$vectors %*%  D %*% t(eigencov$vectors)
#print("d")
bighubnum<-1
counts<- (counts+1)
#print(paste(counts, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))

#print(dim(covmats))
#print('ss')
#print(dim(rnorm(nn)))
#print(dim(covmats %*% rnorm(nn)))
simudat<-matrix(0,samples,nn)   # change n = 300, keep p = 100
for (i in 1:samples){     #??????????????????????????????????????????????????????????????????????????????/
    if (i <= numhub){
    simudat[i,]<-covmats %*% rnorm(nn)         # * (bigwgt *5 ) #((bigwgt-1)*0.5+0.05)     #* ((i%%5)+1) * (bigwgt^((i%%5)==1))
    }
    else {
    simudat[i,]<-covmats %*% rnorm(nn)             # * smallwgt
    }
}
##############################################################################################################################################

#print('ssssssssssssssssssddss')

fixed <- 'fix'
temp<-c()

#print(simudat)
#print( simudat[,1:5] * 5)
#sdlfjslkdfjsdlkjf

haha<-simudat # right before cases 

#case1
if (case ==1){

print(dim(simudat))

simudat[,1:numhub] <- simudat[,1:numhub] * bigwgt   # ((bigwgt-1) *3 +1)            
}
else if (case ==2){
#case2
simudat[,1:floor(numhub/2)] <- simudat[,1:floor(numhub/2)] * bigwgt   # ((bigwgt-1) *3 +1)
simudat[,(numhub+1):(numhub+numhub-floor(numhub/2))] <- simudat[,(numhub+1):(numhub+numhub-floor(numhub/2))] * bigwgt  # ((bigwgt-1) *3 +1)
}
else if (case ==3){
#case3
simudat[,(numhub+1):(numhub+numhub)] <- simudat[,(numhub+1):(numhub+numhub)] * bigwgt  # ((bigwgt-1) *3 +1)
#simudat[,1:floor(numhub/2)] <- simudat[,1:floor(numhub/2)] * bigwgt           # ((bigwgt-1) *3 +1)
#simudat[,(numhub+1):(numhub+floor((nn-numhub)/2))] <- simudat[,(numhub+1):(numhub+floor((nn-numhub)/2))] * bigwgt    # ((bigwgt-1) *3 +1)
}
else if (case ==4){  #random
temp<-sample(nn,numhub)
simudat[,temp] <- simudat[,temp] * bigwgt      # ((bigwgt-1) *3 +1)
}
else if (case ==5){
simudat[,sample(nn,(nn/2))] <- simudat[,sample(nn,nn/2)] * bigwgt   #((bigwgt-1) *3 +1)
}

##########
#print('jjj')
s<-(simudat)
#print(class(s))
simufilename=paste("simudata-","nn",nn,"sp",samples,"nh",numhub,"bighub",bighub,"shlb",shlb,"shub",shub,"bigwgt",bigwgt,".txt", sep='')
#write.table(s,simufilename)
conxfilename=paste("originconx","nn",nn,"sp",samples,"nh",numhub,"bighub",bighub,"shlb",shlb,"shub",shub,"bigwgt",bigwgt,".txt", sep='')
#write.table(conx,conxfilename)
newList <- list("simudata" = s, "originconx" = conx, "rawsimudata"=haha)
write(conx,'origin.txt',ncol=nn)
return(newList)
}
for (iii in 1:5){
#SIMUGENERATE(nn=40,samples=120,case=iii, numhub=5, bighub=20, shlb=0, shub=2,bigwgt=6, disconnected=16)
}
#SIMUGENERATE(nn=16,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=100,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=250,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=625,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=15,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=30,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=45,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=60,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=75,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=90,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=105,case=1, numhub=1, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=2, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=3, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=4, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=6, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=7, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=8, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=9, bighub=8, shlb=1, shub=2,bigwgt=6)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=1)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=3)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=9)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=12)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=15)
#SIMUGENERATE(nn=40,samples=120,case=1, numhub=5, bighub=8, shlb=1, shub=2,bigwgt=18)
























































































