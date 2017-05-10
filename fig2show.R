
fig2show <- function(nn){
nn<-1000

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



a<-(read.table("simuaictpwgt16n5-12case1.txt", header = FALSE))
b<-(read.table("simuaictpwgt16n5-12case2.txt", header = FALSE))
c<-(read.table("simuaictpwgt16n5-12case3.txt", header = FALSE))
d<-(read.table("simuaictpwgt16n5-12case4.txt", header = FALSE))
e<-(read.table("simuaictnwgt16n5-12case1.txt", header = FALSE))
f<-(read.table("simuaictnwgt16n5-12case2.txt", header = FALSE))
g<-(read.table("simuaictnwgt16n5-12case3.txt", header = FALSE))
h<-(read.table("simuaictnwgt16n5-12case4.txt", header = FALSE))

a<-(a+e)/2
b<-(b+f)/2
c<-(c+g)/2
d<-(d+h)/2
#a[1,]<-0.633
#b[1,]<-0.633
#c[1,]<-0.633
#d[1,]<-0.633 
print(a)
nnn<-length(t(a))
completey <- c()
for (i in 1:nnn) {
    completey <- c(completey, a[i,1],b[i,1],c[i,1],d[i,1])
}

# plot lines
test = data.frame(x =  rep(1:nnn, each = 4),
                  group =  rep(c("Scenario 1","Scenario 2","Scenario 3",'Scenario 4')),
                  groupcd= rep(c(1,2,3,4)),
                  y= completey
                  )

    #GGPLOT
p2<-    qplot(x=x, y=y,
           data=test,
           colour=group,
	   shape=group,
           main="") +geom_line()+scale_x_continuous(breaks=seq(1,31,2),labels=c(1:16))
p2<-p2 + labs(x = "Weight")
p2<-p2 + labs(y = "Accuracy")+ theme(plot.title = element_text(size=8.6),legend.title = element_blank())
#tiff(filename="0223caseresults2.jpg")
tiff(filename="figure2.tiff",width = 4000, height = 3200, units = "px", res = 800, compression = 'lzw')
multiplot(p2,cols=1)

hiyeah<-c()
hiyeah<-cbind(a,b,c,d)
write.csv(hiyeah, 'scenarios.csv')


}