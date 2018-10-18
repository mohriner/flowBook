source("Scripts/AnalysisScripts/GrooveSwapDistance.r")
segments = fread("DerivedData/corpus_groove_segments.txt",header=T)

PlotGrooveSegmentation = function(
  v,r.limit = .125,
  plot.all.costs = T, 
  offset=0,
  x.max = 1.01,
  # plot.complexity = F,
  # plot.disjuncture = F,
  next.level = NA,
  new.quartz = T,
  mute.adherence = F)
{
  n.beats = corpora %>% filter(verse==v) %>% 
    .$beatIndex %>% max
  n.bars = ceiling(n.beats/4)
  if(plot.all.costs==T){
    r.limits = c(0,.0625, .125,.25)
  }else{
    r.limits = r.limit
  }
  if(new.quartz==T){
    if(plot.all.costs==T){
      quartz(width=4.55,height=1.25*4)
      par(mfcol=c(4,1),mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
    }else{
      quartz(width=4.55,height=1.25*length(r.limits))
      par(mfcol=c(length(r.limits),1),mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
    }
  }
  
  
  llply(r.limits,function(i){
    x = segments %>% filter(verse==v,effort.rate==i)
    grooveOffsets = as.numeric(x$end)/max(as.numeric(x$end),na.rm=T)
    grooveOnsets = as.numeric(x$start)/max(as.numeric(x$end),na.rm=T)
    plot(x = grooveOffsets,
         y = rep(0,dim(x)[1]),
         xlim=c(0,x.max),ylim=c(-.15,.25),
         xaxt="n",yaxt="n",
         adj=0,pch="|",
         xlab="",ylab="")
    title(main=paste("Accent swaps per bar: ",i*16,sep=""),adj = 0, font.main=3)
    
    # Draw measures
    bars.x = seq(0,1,length.out = n.bars+1)
    
    # Add hyper-measure bars
    for(j in 1:(n.bars %/% 4)){
      lines(rep(bars.x[(j*4)+1],2),c(-.25,.25),col=gray(.75),lty=2,lwd=.5)
    }
    points(0,0,pch="|")
    lines(c(0,1),c(0,0))
    text(bars.x,-.1,1:(n.bars+1),font=3,family="Times New Roman",cex=.75)
    
    
    # Plot label of groove class
    s1 = x %>% 
      select(class,rotation) %>% 
      t 
    g1 = apply(s1,2,function(i) paste(i,collapse="-"))
    if("NA-NA" %in% g1){g1[g1=="NA-NA"] = ""}
    # Determine position of label (in center of groove)
    midpoints = rollapply(c(0,grooveOffsets),2,mean)
    
    text(x = midpoints-.03,
         y = .05,
         labels = g1,
         srt=45,pos=4,
         family="Times New Roman")
    
    # Thicken the baseline for grooves the same as the next level
    if(is.na(next.level)){next.level = c(0,.0625,.125,.25)[c(0,.0625,.125,.25)>i][1]}
    
    if(mute.adherence==F){
      for(j in 1:dim(x)[1]){
        supergroove = segments %>% 
          filter(verse==v,effort.rate == next.level,
                 (start<= (x$start[j]+8)  & end>= (x$end[j]-8)),
                 class==x$class[j],rotation == x$rotation[j])
        if(dim(supergroove)[1]>0){
          lines(c(x$start[j]/max(x$end),x$end[j]/max(x$end)),
                rep(0,2),
                lwd=2)
        }
      }
    }
    
  },.inform=T)
}
# PlotGrooveSegmentation("business1")
