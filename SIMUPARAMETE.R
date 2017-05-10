set.seed(16)
SIMUPARAMETE <- function(simudat,nn,samples,case, numhub, bighub, shlb, shub,bigwgt,disconnected,conx){

#case1
if (case ==1){

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
newList <- list("simudata" = s, "originconx" = conx)
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
























































































