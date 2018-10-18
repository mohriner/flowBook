PlotSet = function(set,card,launch.quartz = TRUE,width=3){
  if(launch.quartz == TRUE){
    quartz(width=width,height=width)
    par(mar = rep(0,4))
  }
  bgs = rep("white",card)
  bgs[set+1] = "black"
  lim=1.2
  x1 = sin(seq(0,2*pi,.01))
  y1 = cos(seq(0,2*pi,.01))
  x2 = sin(seq(0,2*pi,(2*pi)/card))
  y2 = cos(seq(0,2*pi,(2*pi)/card))
  
  
  plot(x1,y1,type="l",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",xlim = lim*c(-1,1),ylim = lim*c(-1,1))
  points(x2,y2,pch=21,bg=bgs,cex=2)
  text(x2*1.2,y2*1.2,0:(card-1),family="Times New Roman", cex=.75)
  polygon(x2[set+1],y2[set+1],lwd=3)
}
AddLine = function(dyad,card,lty = 2){
  a = 1
  x2 = sin(seq(0,2*pi,(2*pi)/card))
  y2 = cos(seq(0,2*pi,(2*pi)/card))
  lines(x2[dyad+1]*a,y2[dyad+1]*a,lty = lty)
}
AddLabel = function(set,rot){
  text(0,0,pos=3,labels=set,family="Times New Roman")
  text(0,0,pos=1,labels=paste("starting on",rot),family="Times New Roman")
}

quartz(width=6,height=3)
par(mfrow=c(1,2))
PlotSet(0:7,7,launch.quartz = T)
text(sin(seq(0,2*pi,(2*pi)/7))*1.2,cos(seq(0,2*pi,(2*pi)/7))*1.2,LETTERS[c(3:7,1:2)],family="Times New Roman", cex=.75)
