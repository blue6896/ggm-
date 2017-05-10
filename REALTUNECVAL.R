library("QUIC", lib.loc="/ihome/jwoo/glasso")
library(QUIC)

REALTUNECVAL<- function(s,nn,samples, valofmaxIter,multirho,valoftol,splits,lbrho,ubrho){
#print(s)
#data<-as.matrix(read.table("realdata.txt"))
data<-s
conx1_2 <- as.matrix((s != 0) + 0)
minBIC <- .Machine$integer.max
minAIC <- .Machine$integer.max
tempBIC <- 0
BICmat <- c()
tempAIC <- 0
AICmat <- c()
minIndex <- 0
minIndexBIC <- 0
temptpratearray <-c()
tempC<- matrix(0,nn,nn)
tempsol<- matrix(0,nn,nn)
tempAbar <- matrix(0,nn,nn)
factors <- 1
#print("e")

simudat<- as.matrix(data)
samples <- length(data[,1])
#print(samples)
#print('dddddddddddddddddddddddddddddddd')
optimumitr <- 0
ss<-1
overallminAIC<-.Machine$integer.max
check <- 1
checkBIC <-1
continuepoint<- 10000
thresh <- 0 # for concentration matrix
startid<-0
endid<-0
alters<-s
 
for (j in 1:multirho){
    continuepoint<-  valofmaxIter
    for (optitr in  valofmaxIter: valofmaxIter){
 for (kfold in 1:splits){
    rhoval <- lbrho + (ubrho-lbrho)*(j)/multirho
    nk<- round((dim(data)[1])/splits*kfold)-round((dim(data)[1])/splits*(kfold-1))
    startid<-round((dim(data)[1])/splits*(kfold-1))+1
    endid<- round((dim(data)[1])/splits*kfold)
    alters<- cov(data[-(startid:endid),])
    #det tr
    a<-QUIC(alters, maxIter=optitr ,rho=rhoval,tol=valoftol)
    Clamk <- a$X
    Ak <- cov(as.matrix(data[startid:endid,]))
    tempC <- as.matrix((abs(a$X) >thresh) + 0)
        diag(tempC)<-0
    #tempAIC <- -2*(a$regloglik) + 2*(sum(tempC))
    if(startid != endid){
    	   #    print(dim(Clamk))
	   #    print((Ak))
        tempAIC<-tempAIC+ nk*(-log(det(Clamk))+sum(diag(Clamk %*% Ak)))
}
else {
      tempAIC<-tempAIC+ nk*(-log(det(Clamk)))
}
#        tempBIC <- -2*(a$regloglik) + log(samplenum)*(sum(tempC))

}
        #tempAIC <-(-log(det(a$X))  + sum(diag(a$X * tempAbar)) + (1/(ss*765))*0.5*(sum(tempC)-100))
        #tempBIC <- (-log(det(a$X))  + sum(diag(a$X * tempAbar)) + (log(765)/(ss*765))*0.5*(sum(tempC)-100))
        AICmat <- c(AICmat, tempAIC)
        BICmat <- c(BICmat, tempBIC)
 #            print(j)
 #            print(minIndex)
 #   print(tempAIC)
 #   print(minAIC)
 #   print(tempBIC)
 #   print(minBIC)

 #   print(a$regloglik)
        if ((abs(tempAIC-minAIC) <0.01)&&(check==1)){
        minIndex <- rhoval
        optimumitr <- optitr
        overallminAIC <- minAIC
        check <-0
        continuepoint <- j
        }
         if ((abs(tempBIC-minBIC) <0.01)&&(checkBIC==1)){
        minIndexBIC <- rhoval
        optimumitr <- optitr
        checkBIC <-0
        continuepoint <- j
        }



        minAIC <- tempAIC
        minBIC <- tempBIC
        if(overallminAIC>=minAIC){   # now find the max?
        #minIndex <- rhoval
        #optimumitr <- optitr
        overallminAIC <- minAIC
        }
        tempAIC<-0
        tempBIC<-0
        tempAbar  <- matrix(0,nn,nn)
}

}

#print('f')

maxIndex <- (which(temptpratearray==max(temptpratearray))[1])
#print(optimumitr)
#print (minIndex)
#print(minIndexBIC)
optimumitr <-10000
rhoind <- (which(AICmat==min(AICmat))[1])
minIndex<- lbrho + (ubrho-lbrho)*(rhoind)/multirho   #                   1/((which(AICmat==min(AICmat))[1]))

#print('optimal')
  cvre<-QUIC(cov(data), maxIter=valofmaxIter ,tol=valoftol,rho=minIndex)
  cvre <- as.matrix((cvre$X != 0) + 0)
  diag(cvre)<-0
   write( cvre, 'CVPRECISION.txt', ncol=nn)

return(minIndex)
}
#s<-as.matrix(read.table("realdata.txt"))
#a<-REALTUNECVAL(s,nn=40,samples=120, valofmaxIter=10000,multirho=100,valoftol=0.0001,splits=5,lbrho=0.5, ubrho=7.5)
#print(a)