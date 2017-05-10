#install.packages('QUIC_1.1.tar.gz')

set.seed(16)

#library(QUIC)
source('SIMUGENERATE.R',print.eval=FALSE)
source('SIMUPARAMETE.R',print.eval=FALSE)
source('SIMUFINDTUNE.R',print.eval=FALSE)
source('SIMUFINDTPTN.R',print.eval=FALSE)
source('REALTUNECVAL.R',print.eval=FALSE)
#source('SIMUNETWORKS.R',print.eval=FALSE)
source('fig1show.R',print.eval=FALSE)
source('fig2show.R',print.eval=FALSE)

aictp<-c()
aictn<-c()
aicacc<-c()
bictp<-c()
bictn<-c()
bicacc<-c()
cvtp<-c()
cvtn<-c() 
cvacc<-c()
cv10tp<-c()
cv10tn<-c()
cvalltp<-c()
cvalltn<-c()
repe <-500# it indicates the number of replications 
aictpval<-0
aictnval<-0
aicaccval<-0
bictpval<-0
bictnval<-0
bicaccval<-0
cvtpval<-0
cvtnval<-0 
cvaccval <-0
cv10tpval<-0
cv10tnval<-0
cvalltpval<-0
cvalltnval<-0
nn<- 40 # 40
samples<-120 #120

#s<-SIMUGENERATE(nn,samples,case=1, numhub=5, bighub=12, shlb=0, shub=2,bigwgt=1,16)  # bighub was 20
ss<-SIMUGENERATE(40,120,case=1, numhub=5, bighub=12, shlb=0, shub=2,bigwgt=((1-1)*0.5+1),16)


for (ca in 1:4){
for (i in 1:31){  # 31 before #250 before  back to 16?
#a<-SIMUGENERATE(nn,samples,case=1, numhub=5, bighub=8, shlb=0, shub=2,bigwgt=(i*0.02+1),10)
#a<-SIMUGENERATE(nn,samples,case=1, numhub=6, bighub=12, shlb=1, shub=2,bigwgt=(i*0.02+1),10)   #with 250 from 1 to 6 
#a<-SIMUGENERATE(nn,samples,case=ca, numhub=5, bighub=20, shlb=0, shub=2,bigwgt=((i-1)*0.5+1),16)
#a<-SIMUGENERATE(nn,samples,case=ca, numhub=i, bighub=20, shlb=0, shub=2,bigwgt=((1-1)*0.5+1),16) #numhub
#a<-SIMUGENERATE(nn,samples,case=ca, numhub=5, bighub=20, shlb=0, shub=2,bigwgt=((i-1)*0.5+1),16) 

a<-SIMUPARAMETE(ss$rawsimudata,nn,samples,case=ca, numhub=5, bighub=12, shlb=0, shub=2,bigwgt=((i-1)*0.5+1),16,ss$originconx)

#a<-SIMUGENERATE(nn,samples,case=ca, numhub=5, bighub=16, shlb=0, shub=2,bigwgt=((i-1)*0.5+1),16)  # bighub was 20 

#a<-SIMUGENERATE(nn,samples,case=ca, numhub=5, bighub=12, shlb=0, shub=2,bigwgt=((i-1)*0.5+1),16)  # bighub was 20
#ddd
#print(a$simudata)
b<-SIMUFINDTUNE(cov(a$simudata),nn,samples, valofmaxIter=10000,multirho=1000,valoftol=0.0001, lbrho=0.0, ubrho=1.0)
b_cv<-REALTUNECVAL((a$simudata),nn,samples, valofmaxIter=10000,multirho=1000,valoftol=0.0001,splits=5,lbrho=0.1, ubrho=6)
b_cv10 <-REALTUNECVAL((a$simudata),nn,samples, valofmaxIter=10000,multirho=1000,valoftol=0.0001,splits=10,lbrho=0.1, ubrho=6)
#b_cvall <- REALTUNECVAL((a$simudata),nn,samples, valofmaxIter=10000,multirho=1000,valoftol=0.0001,splits=samples,lbrho=0.1, ubrho=6)
#print(b$aicparam)
#print(b$bicparam)
#print(b_cv)
#print(b_cv10)

for (j in 1:repe){
#print('why')
#print(j)
cc<-SIMUFINDTPTN(cov(a$simudata),a$originconx,nn,minIndex=b$aicparam, minIndexBIC=b$bicparam,minIndexCV=b_cv, valofmaxIter=10000, valoftol=0.0001)
d<-SIMUFINDTPTN(cov(a$simudata),a$originconx,nn,minIndex=b$aicparam, minIndexBIC=b$bicparam,minIndexCV=b_cv10, valofmaxIter=10000, valoftol=0.0001)
#e<-SIMUFINDTPTN(cov(a$simudata),a$originconx,nn,minIndex=b$aicparam, minIndexBIC=b$bicparam,minIndexCV=b_cvall, valofmaxIter=10000, valoftol=0.0001)
aictpval<- aictpval +  cc$AICTP
aictnval<- aictnval +  cc$AICTN
aicaccval<- aicaccval+ cc$AICACC
bictpval<- bictpval +  cc$BICTP
bictnval<- bictnval +  cc$BICTN
bicaccval<-bicaccval+ cc$BICACC
cvtpval<- cvtpval +  cc$CVTP
cvtnval<- cvtnval +  cc$CVTN
cvaccval<-cvaccval+ cc$CVACC
cv10tpval<- cv10tpval +  d$CVTP
cv10tnval<- cv10tnval +  d$CVTN
#cvalltpval<- cvalltpval +  e$CVTP
#cvalltnval<- cvalltnval +  e$CVTN
#write.table(b$rtconx,"resultconxnew.txt")
#write.table(b$rtconxBIC,"resultconxBICnew.txt")
}
aictp <- c(aictp, aictpval/repe)
aictn <- c(aictn, aictnval/repe)
aicacc<-c(aicacc, aicaccval/repe)
bictp <- c(bictp, bictpval/repe)
bictn <- c(bictn, bictnval/repe)
bicacc<- c(bicacc, bicaccval/repe)
cvtp <- c(cvtp, cvtpval/repe)
cvtn <- c(cvtn, cvtnval/repe)
cvacc<- c(cvacc, cvaccval/repe)
cv10tp <- c(cv10tp, cv10tpval/repe)
cv10tn <- c(cv10tn, cv10tnval/repe)
#cvalltp <- c(cvalltp, cvalltpval/repe)
#cvalltn <- c(cvalltn, cvalltnval/repe)

aictpval<-0
aictnval<-0
aicaccval<-0
bictpval<-0
bictnval<-0
bicaccval<-0 
cvtpval<-0
cvtnval<-0
cvaccval<-0
cv10tpval<-0
cv10tnval<-0
#cvalltpval<-0
#cvalltnval<-0

}


#print(aictp)
#print(aictn)
#print(aicacc)
#print(bictp)
#print(bictn)
#print(bicacc)
#print(cvtp)
#print(cvtn)
#print(cvacc)
#print(cv10tp)
#print(cv10tn)
write(aictp, paste('simuaictpwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(aictn,  paste('simuaictnwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(aicacc,  paste('simuaicaccwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(bictp,  paste('simubictpwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(bictn,  paste('simubictnwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(bicacc,  paste('simubicaccwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(cvtp,  paste('simucvtpwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(cvtn,  paste('simucvtnwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(cvacc,  paste('simucvaccwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(cv10tp,  paste('simucv10tpwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
write(cv10tn,  paste('simucv10tnwgt16n5-12case',ca,'.txt', sep=''),ncol=1)
aictp<-c()
aictn<-c()
aicacc<-c()
bictp<-c()
bictn<-c()
bicacc<-c()
cvtp<-c()
cvtn<-c()
cvacc<-c()
cv10tp<-c()
cv10tn<-c()

}

fig1show(1000)
fig2show(10000)


#print(cvalltp)
#print(cvalltn)

#kohda<-as.matrix(read.table("AICPRECISION2.txt")) # result with AIC
#filename1<-'aicgraphresult3-2.jpg'
#t<-as.matrix(read.table("BICPRECISION2.txt"))  # result with BIC
#filename2<-'bicgraphresult3-2.jpg'
#cvval<- as.matrix(read.table("CVPRECISION2.txt"))  # result with CV
#filename3<-'cvgraphresult3-2.jpg'
#u<-as.matrix(read.table("origin2.txt"))     # original conx
#print(kohda)
#print(u)
#SIMUNETWORKS(kohda, filename1, u)
#SIMUNETWORKS(t, filename2, u)
#SIMUNETWORKS(cvval, filename3, u)
#print('b-hubhas6edges-20161023')