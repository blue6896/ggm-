#library("QUIC", lib.loc="/ihome/jwoo/glasso")
library(QUIC)
replication<-1

SIMUFINDTPTN<-function(s,t,nn,minIndex, minIndexBIC, minIndexCV, valofmaxIter, valoftol){
conx<-t
#print(conx)
tp <- c()
tn <- c()
fp <- c()
fn <- c()
tpr<-c()
tnr<-c()
fpr<-c()
fnr<-c()
acc<-c()

tprate <- 0
tnrate <- 0
fprate <- 0
fnrate <- 0
accuracy <- 0
tprate2 <- 0
tnrate2 <- 0
fprate2 <- 0
fnrate2 <- 0
accuracy2 <- 0
tp2 <- c()
tn2 <- c()
fp2 <- c()
fn2 <- c()
tpr2 <-c()
tnr2 <-c()
fpr2 <-c()
fnr2 <-c()
acc2 <-c()
tprate3 <- 0
tnrate3 <- 0
fprate3 <- 0
fnrate3 <- 0
accuracy3 <- 0
tp3 <- c()
tn3 <- c()
fp3 <- c()
fn3 <- c()
tpr3 <-c()
tnr3 <-c()
fpr3 <-c()
fnr3 <-c()
acc3 <-c()


a<-QUIC(s, maxIter=valofmaxIter ,tol=valoftol, rho=minIndex)
a2<-QUIC(s, maxIter=valofmaxIter ,tol=valoftol, rho=minIndexBIC)
a3<-QUIC(s, maxIter=valofmaxIter ,tol=valoftol, rho=minIndexCV)
#print(a$X)
conx2 <- as.matrix((a$X != 0) + 0) ############################ != or ==
conx2BIC <- as.matrix((a2$X != 0) + 0) ############################ != or ==
conx2CV <- as.matrix((a3$X != 0) + 0) ############################ != or ==
#write.table((a$X), file="mymatrix.txt", row.names=FALSE, col.names=FALSE)

##################################simulation bootstrap ###############################
consistcheck<- c()
consistdistribution<- c()
rpt <- 1

for (kval in 1:rpt){

samplenum<-nn
hi<-matrix(0,nn,nn)
tempax<-matrix(0,nn,nn)
tempaxconsist<-matrix(0,nn,nn)
tempaxconsist0<-matrix(0,nn,nn)
tempaxconsist1<-matrix(0,nn,nn)
postempax<- matrix(0,nn,nn)
tempa2x<-matrix(0,nn,nn)
tempa3x<-matrix(0,nn,nn)
bootnum<-1
for (cycle in 1:bootnum){
sam<-sample(samplenum, replace=T)
#tempdata<-simudat[sam,]

a<-QUIC(s, maxIter=valofmaxIter ,tol=valoftol, rho=minIndex)
a2<-QUIC(s, maxIter=valofmaxIter ,tol=valoftol, rho=minIndexBIC)  ################################### BIC variable?
a3<-QUIC(s, maxIter=valofmaxIter ,tol=valoftol, rho=minIndexCV)
postempax <- postempax #+ (a$X)
tempax <- tempax+ as.matrix(a$X!=0)
tempa2x <-tempa2x + as.matrix(a2$X!=0)
tempa3x <-tempa3x + as.matrix(a3$X!=0)
}

for (aindex in 1:nn){
    for (bindex in 1:nn){
if (conx[aindex, bindex]==0){
tempaxconsist0[aindex, bindex] <- (bootnum-tempax[aindex, bindex])/bootnum
 tempaxconsist1[aindex, bindex] <-0
}
else {
     tempaxconsist1[aindex, bindex] <- ((tempax[aindex, bindex])/bootnum)
      tempaxconsist0[aindex, bindex] <-0
}
}
}
todivide0<- sum(hi+conx==0)-nn
todivide1<- sum(hi+conx!=0)

diag(tempax)<-0
diag(tempaxconsist)<-0
if(kval==1){
#print(tempax)
#print(tempaxconsist)
#write(tempaxconsist, paste('consistency','case',case ,'bigweight',bigwgt, '.txt',collapse='', sep=''), ncol=40)
#write(sum(tempaxconsist)/(nn*nn-nn),paste('valueofconsistency','case',case ,'bigweight',bigwgt, '.txt',collapse='', sep=''), ncol=1)
#paste('valueofconsistency','case',case ,'bigweight',bigwgt, '.txt',collapse='', sep=''), ncol=1 
#print(max(tempaxconsist))
#print(which(tempaxconsist == max(tempaxconsist), arr.ind = TRUE))
ll <- (which(tempaxconsist == max(tempaxconsist), arr.ind = TRUE))
#write(ll, paste('maxindex','case',case ,'bigweight',bigwgt, '.txt',collapse='', sep=''))
}

hi<- hi+ (tempaxconsist == max(tempaxconsist))
if(kval==1){
#write(hi, paste('maxconsistency','case',case ,'bigweight',bigwgt, '.txt',collapse='', sep=''),ncol=40)
}
consistdistribution<- c(consistdistribution, sum(tempax==max(tempax)))
#consistcheck<- c(consistcheck,sum(tempaxconsist)/todivide)

}

#print(consistdistribution)
#write ((consistdistribution/2), paste('consistdistribution','case',case ,'bigweight',bigwgt,'.txt',collapse='', sep=''), ncol=1)
#jpeg(paste('consistdistribution','case',case ,'bigweight',bigwgt, '.jpg',collapse='', sep=''))
#hist(t(consistdistribution))
#print(consistcheck)
#write (consistcheck,'consistcheck.txt', ncol=1)


#for(bootthres in 2:2){
#bootthres <- 25+ (bootthres*5)
#print(bootthres)
#for (k in 1:1){
##conx2 <- as.matrix((abs(a$X) > (thresh + (k-1)/500)) + 0) ############################ != or ==
#conx2 <- as.matrix(tempax>=(bootthres*5))
#}}
############################################################################################

elem <-c()

diag(conx2) <- 100
diag(conx2BIC) <- 100
diag(conx2CV)<-100
###############################################################  change !! nothing -> [1:5,] 
#solution <- (conx[1:5,1:40] *2 + conx2[1:5,1:40])
#solution2 <- (conx[1:5,1:40] *2 + conx2BIC[1:5,1:40])
#solution3 <- (conx[1:5,1:40] *2 + conx2CV[1:5,1:40])
solution <- (conx *2 + conx2)
solution2 <- (conx *2 + conx2BIC)
solution3 <- (conx *2 + conx2CV)
##################################################################
#write.table((conx2), file="mat.txt", row.names=FALSE, col.names=FALSE) #### matrix contents
#write.table(((solution%%3)==0)+0,file="mat10412correct.txt", row.names=FALSE, col.names=FALSE)
names <- table (solution)
#print(names)

tp <- c(tp,sum(solution==3))
fp <- c(fp,sum(solution==1))
fn <- c(fn,sum(solution==2))
tn <- c(tn,sum(solution==0)-nn)

#print(tp)
#print(tn)
#print(fp)
#print(fn)
#print(minIndex)

tprate <- tprate + (sum(solution==3)/(sum(solution==3)+sum(solution==2)))
tnrate <- tnrate + (sum(solution==0)/(sum(solution==1)+sum(solution==0)))
fprate <- fprate +(sum(solution==2)/(sum(solution==3)+sum(solution==2)))
fnrate <- fnrate + (sum(solution==1)/(sum(solution==1)+sum(solution==0)))
accuracy <-  accuracy+(sum(solution==3)+sum(solution==0))/(sum(solution==3)+sum(solution==2)+sum(solution==1)+sum(solution==0))

# BIC starts

tp2 <- c(tp2,sum(solution2==3))
fp2 <- c(fp2,sum(solution2==1))
fn2 <- c(fn2,sum(solution2==2))
tn2 <- c(tn2,sum(solution2==0)-nn)

#print(tp2)
#print(tn2)
#print(fp2)
#print(fn2)
#print(minIndexBIC)

tprate2 <- tprate2 + (sum(solution2==3)/(sum(solution2==3)+sum(solution2==2)))
tnrate2 <- tnrate2 + (sum(solution2==0)/(sum(solution2==1)+sum(solution2==0)))
fprate2 <- fprate2 +(sum(solution2==2)/(sum(solution2==3)+sum(solution2==2)))
fnrate2 <- fnrate2 + (sum(solution2==1)/(sum(solution2==1)+sum(solution2==0)))
accuracy2 <-  accuracy2+(sum(solution2==3)+sum(solution2==0))/(sum(solution2==3)+sum(solution2==2)+sum(solution2==1)+sum(solution2==0))

# CV starts 

# BIC starts

tp3 <- c(tp3,sum(solution3==3))
fp3 <- c(fp3,sum(solution3==1))
fn3 <- c(fn3,sum(solution3==2))
tn3 <- c(tn3,sum(solution3==0)-nn)

#print(tp3)
#print(tn3)
#print(fp3)
#print(fn3)
#print(minIndexCV)

tprate3 <- tprate3 + (sum(solution3==3)/(sum(solution3==3)+sum(solution3==2)))
tnrate3 <- tnrate3 + (sum(solution3==0)/(sum(solution3==1)+sum(solution3==0)))
fprate3 <- fprate3 +(sum(solution3==2)/(sum(solution3==3)+sum(solution3==2)))
fnrate3 <- fnrate3 + (sum(solution3==1)/(sum(solution3==1)+sum(solution3==0)))
accuracy3 <-  accuracy3+(sum(solution3==3)+sum(solution3==0))/(sum(solution3==3)+sum(solution3==2)+sum(solution3==1)+sum(solution3==0))



 # for replications sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


tpr <- c(tpr, tprate/replication)
tnr <- c(tnr, tnrate/replication)
fpr <- c(fpr, fprate/replication)
fnr <- c(fnr, fnrate/replication)
acc <- c(acc, accuracy/replication)
tprate <-0
tnrate <-0
fprate <-0
fnrate <-0
accuracy <-0

tpr2 <- c(tpr2, tprate2/replication)
tnr2 <- c(tnr2, tnrate2/replication)
fpr2 <- c(fpr2, fprate2/replication)
fnr2 <- c(fnr2, fnrate2/replication)
acc2 <- c(acc2, accuracy2/replication)
tprate2 <-0
tnrate2 <-0
fprate2 <-0
fnrate2 <-0
accuracy2 <-0

tpr3 <- c(tpr3, tprate3/replication)
tnr3 <- c(tnr3, tnrate3/replication)
fpr3 <- c(fpr3, fprate3/replication)
fnr3 <- c(fnr3, fnrate3/replication)
acc3 <- c(acc3, accuracy3/replication)
tprate3 <-0
tnrate3 <-0
fprate3 <-0
fnrate3 <-0
accuracy3 <-0

names <- table (solution2)
#print(names)

#print(tpr)
#print(tnr)
#print(fpr)
#print(fnr)
#print(acc)
#print('important above')

dateinfo<- gsub(" ", "", date(), fixed = TRUE)
#hiaictpr<-paste(nn,"nodes",numhub, "numhub",bighub,"bighub",shlb, "shlb",shub,"shub", "aictpr",case ,fixed,".txt",collapse='', sep='')
#hiaictnr<-paste(nn,"nodes",numhub, "numhub",bighub,"bighub", shlb, "shlb",shub,"shub", "aictnr",case ,fixed,".txt",collapse='', sep='')
#tprfile<-  hiaictpr
#tnrfile<-  hiaictnr
#accfile<-  "/ihome/jwoo/glasso/aaaaaaaicacc.txt"
##write(tpr, tprfile, ncol=5,append=T)
##write(tnr, tnrfile, ncol=5,append=T)
##write(acc, accfile, ncol=5,append=T)

#hibictpr<-paste(nn,"nodes",numhub, "numhub",bighub,"bighub",shlb, "shlb",shub,"shub",  "bictpr",case, fixed,".txt",collapse='', sep='')
#hibictnr<-paste(nn,"nodes",numhub, "numhub",bighub,"bighub",shlb, "shlb",shub,"shub", "bictnr",case, fixed,".txt",collapse='', sep='')
#tprfile<-  hibictpr
#tnrfile<-  hibictnr
#accfile<-  "/ihome/jwoo/glasso/aaaaaabicacc.txt"

##write(tpr2, tprfile, ncol=5,append=T)
##write(tnr2, tnrfile, ncol=5,append=T)
##write(acc2, accfile, ncol=5,append=T)

#print(tpr2)
#print(tnr2)
#print(acc2)
#print(fnr2)
#print(acc2)

#print(tpr3)
#print(tnr3)
#print(acc3)
#print(fnr3)
#print(acc3)


#print(seednum)
#print(replication)
list3 <- list("AICTP"=tpr,"AICTN"=tnr,"BICTP"=tpr2,"BICTN"=tnr2,"CVTP"=tpr3,"CVTN"=tnr3, "AICACC"=acc, "BICACC"=acc2, "CVACC"=acc3)
#,"originconx"=conx, "rtconx"=conx2, "rtconxBIC"=conx2BIC,"rtconxCV"=conx2CV)
return(list3)
}

#s<-as.matrix(read.table("simudata-nn100sp120nh5bighub8shlb1shub2bigwgt6.txt"))
#t<-as.matrix(read.table("originconxnn100sp120nh5bighub8shlb1shub2bigwgt6.txt"))
#u<-SIMUFINDTPTN(s,t,nn=100,minIndex=0.271, minIndexBIC=0.339, valofmaxIter=10000, valoftol=0.0001)
#write.table(u$resultconx,"resultconxnew.txt")
#write.table(u$resultconxBIC,"resultconxBICnew.txt")

#s<-as.matrix(read.table("simutesting.txt"))
#t<-as.matrix(read.table("originconx.txt"))
#u<-SIMUFINDTPTN(s,t,nn=40,minIndex=0.5, minIndexBIC=0.6, valofmaxIter=10000, valoftol=0.0001)
#write.table(u$resultconx,"resultconx.txt")
#write.table(u$resultconxBIC,"resultconxBIC.txt")

