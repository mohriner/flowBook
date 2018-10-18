### The main plotting function ###
plotVerseCombined.special = function(
  dat,
  tight = T, 
  save.plot = F,
  plot.beat.type=F,
  plot.accent = T,
  plot.rhymeClasses = F,
  plot.rhymeFeatures=F,
  black_and_white=T,
  m.range = NA,
  row.index=NA,
  closeScreen = F,
  Width=4.5,
  Meas.height=.35)
{
  
  n.meas = diff(range(dat$c.meas[!is.na(dat$c.meas)]))+1
  meas.mult = .3
  
  if(plot.beat.type==T&tight==F){
    quartz(width = 6,height = n.meas*Meas.height)
  }else{
    quartz(width = Width,height = n.meas*Meas.height)
  }
  par(mar = c(0,0,0,0))
  
  if(plot.beat.type==T){par(fig=c(0,.65,0,1))}
  if(tight==T){dat[,"syllable"] = gsub("_","",dat[,"syllable"])}
  meas = 4 # should either be number of beats or duration in seconds, depending on data
  #meas = 3.09
  plot(0,0,col="white",xlim=c(-.15,meas),
       ylim=rev(range(dat$c.meas[!is.na(dat$c.meas)])+c(-.5,.15)),yaxp=c(n.meas,0,n.meas),
       xaxt="n",bty="n",las=1,mgp=c(1,0,0),tcl=.01,family="Times New Roman",font=3,
       yaxt="n")
  llply(max(dat$c.meas[!is.na(dat$c.meas)]):min(dat$c.meas[!is.na(dat$c.meas)]),function(x) lines(c(0,4),rep(x,2),lwd=.5))	
  
  ###---Layer 1: the meter grid ----
  # Draw the meter: not a graph-paper-like grid
  # get points
  n.meas = length(seq(min(dat$c.meas[!is.na(dat$c.meas)]),max(dat$c.meas[!is.na(dat$c.meas)])))
  xs = rep(seq(0,4,length.out=17),each=2)
  ys = rep(0,34)
  ys[seq(1,33,2)] = c(rep(c(-.25,-.12,-.18,-.12),4),-.25)
  xs2 = rep(NA,17*3)
  xs2[seq(1,16*3+1,by=3)] = xs[seq(1,16*2+1,by=2)]
  xs2[seq(2,16*3+2,by=3)] = xs[seq(2,16*2+2,by=2)]
  ys2 = rep(NA,17*3)
  ys2[seq(1,16*3+1,by=3)] = ys[seq(1,16*2+1,by=2)]
  ys2[seq(2,16*3+2,by=3)] = ys[seq(2,16*2+2,by=2)]
  xs3 = rep(xs2,n.meas)
  ys3 = ys2+rep(seq(min(dat$c.meas,na.rm=T),max(dat$c.meas,na.rm=T)),each=length(ys2))
  line_coords = list(x=xs3,y=ys3)
  # The little dots at the top
  xs = seq(0,4,length.out=17)
  ys= c(rep(c(-.25,-.12,-.18,-.12),4),-.25)
  ys2 = rep(seq(min(dat$c.meas,na.rm=T),max(dat$c.meas,na.rm=T)),each=length(ys)) + rep(ys,length(seq(min(dat$c.meas,na.rm=T),max(dat$c.meas,na.rm=T))))
  xs2 = rep(xs,n.meas)
  point_coords = list(x = xs2,y=ys2)
  lines(line_coords,lwd=.5)
  points(point_coords,pch=20,cex=.25)
  
  ###---Draw the points ----
  # Point size, determined by accent, cex= .5 or 1.25
  p.cex = rep(.5,length(dat[,1]));
  if(is.element("accent",colnames(dat))){p.cex[which(dat[,"accent"]==1)] = 1.25}
  if(plot.accent==FALSE){p.cex = rep(.5,length(dat[,1]))}
  if(black_and_white==TRUE){overridden = rep("black",length(dat[,1]))}
  
  # Lines between them ----
  llply(1:length(dat$c.beat), function(i){
    if(dat$c.meas[i]==dat$q.measure[i]){
      lines(c(dat$c.beat[i],dat$q.beat[i]),c(dat$c.meas[i]-.35,dat$q.measure[i]))
    }
    
  })
  # The continuous points ----
  points(dat[,"c.beat"],dat[,"c.meas"]-.35,cex=p.cex,pch=21,bg="white",col = overridden)
  # the quantized points
  points(dat[,"q.beat"],dat[,"q.measure"],cex=p.cex,pch=21,bg="white",col = overridden)
  
}

# Quantized
dat = data.frame(
  syllable = rep("",16),
  accent = rep(c(1,0,0,0),4),
  c.meas = rep(0,16),
  c.beat = seq(0,3.75,.25),
  q.measure = rep(0,16),
  q.beat = seq(0,3.75,.25)
)
plotVerseCombined.special(dat)
quartz.save(file="Examples/Chapter 8/Example 8.6a.pdf",type="pdf");dev.off()

# Phase only
dat = data.frame(
  syllable = rep("",16),
  accent = rep(c(1,0,0,0),4),
  c.meas = rep(0,16),
  c.beat = seq(0,3.75,.25) + .1,
  q.measure = rep(0,16),
  q.beat = seq(0,3.75,.25)
)
plotVerseCombined.special(dat)
quartz.save(file="Examples/Chapter 8/Example 8.6b.pdf",type="pdf");dev.off()

# Swing only
dat = data.frame(
  syllable = rep("",16),
  accent = rep(c(1,0,0,0),4),
  c.meas = rep(0,16),
  c.beat = seq(0,3.75,.25) + rep(c(0,.1),8),
  q.measure = rep(0,16),
  q.beat = seq(0,3.75,.25)
)
plotVerseCombined.special(dat)
quartz.save(file="Examples/Chapter 8/Example 8.6c.pdf",type="pdf");dev.off()

# Tempo only
dat = data.frame(
  syllable = rep("",16),
  accent = rep(c(1,0,0,0),4),
  c.meas = rep(0,16),
  c.beat = seq(.25,3.75,length.out=17)[1:16],
  q.measure = rep(0,16),
  q.beat = seq(0,3.75,.25)
)
plotVerseCombined.special(dat)
quartz.save(file="Examples/Chapter 8/Example 8.6d.pdf",type="pdf");dev.off()

# Deceleration
l = 13
dat = data.frame(
  syllable = rep("",16)[1:l],
  accent = rep(c(1,0,0,0),4)[1:l],
  c.meas = rep(0,16)[1:l],
  c.beat = seq(0,3.75,length.out=17)[1:l] + (seq(0,1.4,length.out = 17)^2)[1:l],
  q.measure = rep(0,16)[1:l],
  q.beat = seq(0,3.75,.25)[1:l]
)
plotVerseCombined.special(dat)
quartz.save(file="Examples/Chapter 8/Example 8.6e.pdf",type="pdf");dev.off()

remove(list=c("plotVerseCombined.special","l"))
