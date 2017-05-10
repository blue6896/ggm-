#library("QUIC", lib.loc="/ihome/jwoo/glasso")
library(QUIC)

SIMUFINDTUNE<- function(s,nn,samples, valofmaxIter,multirho,valoftol,lbrho, ubrho){

#print(s)
write(s, 'kkkkk.txt')
conx1_2 <- as.matrix((s != 0) + 0)
minBIC <- .Machine$integer.max
minAIC <- .Machine$integer.max
tempBIC <- 0
BICmat <- c()
tempAIC <- 0
AICmat <- c()
minIndex <- 0
temptpratearray <-c()
tempC<- matrix(0,nn,nn)
tempsol<- matrix(0,nn,nn)
tempAbar <- matrix(0,nn,nn)
factors <- 1
#print("e")
continuepoint<- valofmaxIter
overallminAIC<- .Machine$integer.max
optitr <- valofmaxIter
for (j in 1:multirho){                                    ## it was 2:12     ######### it had been 2:200
    continuepoint<- valofmaxIter
################# for iterations
        if (optitr > continuepoint){
    AICmat <- c(AICmat, tempAIC)
#    print(tempAIC)
    next
    }
    else {
#    print(optitr)
    }
###################
#    print(j)
#    print(tempAIC)
#    print(minAIC)
#    print(tempBIC)
#    print(minBIC)
#simudat[,1:numhub] <- simudat[,1:numhub] * bigwgt
##########
#s<-cov(simudat)
#    rhoval <- 1/(j*100)* factors
   rhoval <- 1/(j*j/2)
     rhoval <- lbrho + (ubrho-lbrho)*(j)/multirho

  a<-QUIC(s, maxIter=valofmaxIter ,tol=valoftol,rho=rhoval)
    tempC <- as.matrix((a$X != 0) + 0)
        tempAIC <- -2*(a$regloglik) + 2*(sum(tempC)-100)
        tempBIC <- -2*(a$regloglik) + log(samples)*(sum(tempC)-100)

        BICmat <- c(BICmat, tempBIC)
        AICmat <- c(AICmat, tempAIC)
        minBIC <- tempBIC
        minAIC <- tempAIC

        if(overallminAIC>=minAIC){
        minIndex <- rhoval
        optimumitr <- optitr
        overallminAIC <- minAIC
 #       print('ssssssssssssssssss')
 #       print(minIndex)
 #       print(optimumitr)
        }
        if (abs(tempAIC-minAIC) <0.0000){
        continuepoint <- j
        }
        tempAbar  <- matrix(0,nn,nn)
}

#print('f')
#print(BICmat)
#print(AICmat)
#print(which(BICmat==min(BICmat))[1])
#print(which(AICmat==min(AICmat))[1])
#minIndex <-  1/((((which(AICmat==min(AICmat))[1]+1))^2)/2)
#minIndexBIC <-  1/((((which(BICmat==min(BICmat))[1]+1))^2)/2)
##maxIndex <- (which(temptpratearray==max(temptpratearray))[1])*factors*0.1
rhoind <- (which(AICmat==min(AICmat))[1])
rhoindBIC  <- (which(BICmat==min(BICmat))[1])

minIndex<- lbrho + (ubrho-lbrho)*(rhoind)/multirho   #                   1/((which(AICmat==min(AICmat))[1]))
minIndexBIC<- lbrho + (ubrho-lbrho)*(rhoindBIC)/multirho  #            1/((which(BICmat==min(BICmat))[1]))


#print(optimumitr)
#print(minIndex)
  a<-QUIC(s, maxIter=valofmaxIter ,tol=valoftol,rho=minIndex)
a<- (as.matrix((a$X != 0) + 0))
diag(a)<-0
#print(a)
write(a, 'AICPRECISION.txt', ncol=nn)
  b<-QUIC(s, maxIter=valofmaxIter ,tol=valoftol,rho=minIndexBIC)
b<-(as.matrix((b$X != 0) + 0))
diag(b)<-0
   write(b, 'BICPRECISION.txt', ncol=nn)
#write(AICmat, textname, ncol=100)

newList2<-list("aicparam" = minIndex, "bicparam" = minIndexBIC)
return(newList2)

}

#s<-as.matrix(read.table("simutesting.txt"))
#s<-as.matrix(read.table("simudata-nn100sp120nh5bighub8shlb1shub2bigwgt6.txt"))
#o#nn100sp120nh5bighub8shlb1shub2bigwgt6.txt
#SIMUFINDTUNE(s,nn=40,samples=120, valofmaxIter=10000,multirho=1000,valoftol=0.0001, lbrho=0.0, ubrho=1.0)