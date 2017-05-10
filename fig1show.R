
fig1show <- function(nn){

nn<-10000

library(grid)



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


library(ggplot2)



#11072016simuaicaccwgt16-case1-2.txt        11072016simubicaccwgt16-case1.txt          11072016simucv10tpwgt16-case1-4.txt
#11072016simuaicaccwgt16-case1-3.txt        11072016simubictnwgt16-case1-2.txt         11072016simucv10tpwgt16-case1.txt
#11072016simuaicaccwgt16-case1-4.txt        11072016simubictnwgt16-case1-3.txt         11072016simucvaccwgt16-case1-2.txt
#11072016simuaicaccwgt16-case1.txt          11072016simubictnwgt16-case1-4.txt         11072016simucvaccwgt16-case1-3.txt
#11072016simuaictnwgt16-case1-2.txt         11072016simubictnwgt16-case1.txt           11072016simucvaccwgt16-case1-4.txt
#11072016simuaictnwgt16-case1-3.txt         11072016simubictpwgt16-nn1samples31-2.txt  11072016simucvaccwgt16-case1.txt
#11072016simuaictnwgt16-case1-4.txt         11072016simubictpwgt16-nn1samples31-3.txt  11072016simucvtnwgt16-case1-2.txt
#11072016simuaictnwgt16-case1.txt           11072016simubictpwgt16-nn1samples31-4.txt  11072016simucvtnwgt16-case1-3.txt#
#11072016simuaictpwgt16-nn1samples31-2.txt  11072016simubictpwgt16-nn1samples31.txt    11072016simucvtnwgt16-case1-4.txt
#11072016simuaictpwgt16-nn1samples31-3.txt  11072016simucv10tnwgt16-case1-2.txt        11072016simucvtnwgt16-case1.txt
#11072016simuaictpwgt16-nn1samples31-4.txt  11072016simucv10tnwgt16-case1-3.txt        11072016simucvtpwgt16-nn1samples31-2.txt
#11072016simuaictpwgt16-nn1samples31.txt    11072016simucv10tnwgt16-case1-4.txt        11072016simucvtpwgt16-nn1samples31-3.txt
#11072016simubicaccwgt16-case1-2.txt        11072016simucv10tnwgt16-case1.txt          11072016simucvtpwgt16-nn1samples31-4.txt
#11072016simubicaccwgt16-case1-3.txt        11072016simucv10tpwgt16-case1-2.txt        11072016simucvtpwgt16-nn1samples31.txt
#a<-(read.table("0223simu15140aictpwgt25case1.txt", header = FALSE))
#b<-(read.table("0223simu15140bictpwgt25case1.txt", header = FALSE))
#c<-(read.table("0223simu15140cv10tpwgt25case1.txt", header = FALSE))
#d<-(read.table("0223simu15140cvtpwgt25case1.txt", header = FALSE))
#a2<-(read.table("0223simu15140aictn12wgt25case1.txt", header = FALSE))
#b2<-(read.table("0223simu15140bictn12wgt25case1.txt", header = FALSE))
#c2<-(read.table("0223simu15140cv10tn12wgt25case1.txt", header = FALSE))
#d2<-(read.table("0223simu15140cvtn12wgt25case1.txt", header = FALSE))
a<-(read.table("simuaictpwgt16n5-12case1.txt", header = FALSE))
b<-(read.table("simubictpwgt16n5-12case1.txt", header = FALSE))
c<-(read.table("simucvtpwgt16n5-12case1.txt", header = FALSE))
d<-(read.table("simucv10tpwgt16n5-12case1.txt", header = FALSE))
a2<-(read.table("simuaictnwgt16n5-12case1.txt", header = FALSE))
b2<-(read.table("simubictnwgt16n5-12case1.txt", header = FALSE))
c2<-(read.table("simucvtnwgt16n5-12case1.txt", header = FALSE))
d2<-(read.table("simucv10tnwgt16n5-12case1.txt", header = FALSE))
 
a<-(a+a2)/2
b<-(b+b2)/2
c<-(c+c2)/2
d<-(d+d2)/2

nnn<-length(t(a))
completey <- c()
for (i in 1:nnn) {
    completey <- c(completey, a[i,1],b[i,1],c[i,1],d[i,1])
}

# plot lines
test = data.frame(x =  rep(1:nnn, each = 4),
                  group =  rep(c("AIC","BIC","10-fold CV","5-fold CV")),
                  groupcd= rep(c(1,2,3,4)),
		  y= completey
                  )
    #GGPLOT
p1<-    qplot(x=x, y=y, 
           data=test, 
           colour=group, 
	   shape=group,
           main="") +geom_line()+scale_x_continuous(breaks=seq(1,31,2),labels=c(1:16))+ theme(legend.title = element_blank())

p1<-p1 + labs(x = "Weight")
p1<-p1 + labs(y = "Accuracy")

tiff(filename="figure1.tiff",width = 4000, height = 3200, units = "px", res = 800, compression = 'lzw')
multiplot(p1,cols=1)

hiyeah2<-c()
hiyeah2<-cbind(a,b,c,d)
write.csv(hiyeah2,'methods2.csv')

}