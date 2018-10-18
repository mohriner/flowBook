# This script takes a table of onsets for events in an instrumental loop and 
# creates spiral plots of those event's alignment with the beat in the same
# manner as the spiral plots of segments of rapping. The tabl has four columns:
# onset, instrument, measure (0-indexed), and position (0 through 15). It asserts
# a "zero point" of meter through the boom of the boom bap, i.e., bass drum
# events on positions 0 or 8.
###--- Data processing ----
PlotLoop = function(piece){
  dat = read.table(paste("SourceData/Other/talibKweli_",
                         piece,
                         "_loopEvents.txt",sep=""),stringsAsFactors=FALSE,header=T)
  
  # Find the mean onset of each 0mod8 position
  onBeatMeanOnsets = dat %>% filter(pos%%8==0,inst=="b.drum") %>% 
    group_by(meas,pos) %>% 
    summarize(o = mean(onset)) %>% 
    .$o
  
  # Find the average length of the measure implied 0mod8 events
  meas = onBeatMeanOnsets%>% diff %>% mean %>% multiply_by(2)
  m1.detected =  onBeatMeanOnsets[1]
  diffs = laply(seq(m1.detected-.05,m1.detected+.05,.001),
                function(i){
                  mean((onBeatMeanOnsets - (((meas/2)*0:(length(onBeatMeanOnsets)-1))+i)))
                }) %>% abs
  m1.optimal = seq(m1.detected-.05,m1.detected+.05,.001)[which.min(diffs)]
  
  # Build metric grid from optimal downbeat and average measure length
  n.meas = 4
  meter = seq(m1.optimal,m1.optimal+(meas*n.meas),length.out=n.meas*16 + 1)
  
  bi.c = alply(dat,1,function(i){
    meter1 = meter[i$meas*16 + i$pos + 1]
    onset1 = i$onset
    async = (onset1-meter1)/(meas/4)
    i$meas * 4 + (i$pos/4) + async
  }) %>% unlist %>% unname %>% round(.,digits=2)
  dat = dat %>% cbind(bi.c) %>% mutate(bi.q = meas*4+pos/4)
  ###--- Plotting ----
  quartz(width=4.55,height=6.75)
  par(mfrow = c(3,2), mar=c(0,1,1,0),cex = .65,las=1,bty='n',
      family="Times New Roman")
  llply(c(dat$inst %>% unique %>% sort),function(instrum){
    dat1 = dat %>% filter(inst==instrum)
    # Define x and y limits, number of beats
    z1 = min(ceiling(dat1$bi.c),na.rm=T)-1 # for xlim and ylim
    z2 = max(ceiling(dat1$bi.c),na.rm=T) # for xlim and ylim
    n = z2-z1 # number of beats
    
    adj1 = 4
    plot(0,0,ylim=c(-z2-adj1,z2+adj1),xlim=c(-z2-adj1,z2+adj1),
         col="white",bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
    
    ### Draw background
    # Plot lines through the circle
    Min = matrix(c(15,45,
                   10,40,
                   5,35,
                   0,30,
                   55,25,
                   50,20),
                 2,6)
    for(h in 1:6){
      i = seq(0,1,length.out = 7)[h]
      
      lines(cos(c(pi*i,pi*(i+1)))*(z2+1),
            sin(c(pi*i,pi*(i+1)))*(z2+1),
            lwd=.25)
      
      text(cos(c(pi*i,pi*(i+1)))*(z2+3),
           sin(c(pi*i,pi*(i+1)))*(z2+3),
           labels = paste(":",Min[,h],sep=""))
      
    }
    
    
    # The spiral
    lines(x = rep(cos(-seq(0,2*pi,length.out=1000)+(pi/2)),n)*(seq(z1,z2,length.out=1000*n)+.25),
          y = rep(sin(-seq(0,2*pi,length.out=1000)+(pi/2)),n)*(seq(z1,z2,length.out=1000*n)+.25),
          lwd=.25
    )
    
    ### Divisions of the beat----
    adj1 = 1.25
    text(
      x = c(
        0,
        z2+(3+adj1),
        0,
        -z2-(3+adj1)
      ),
      y = c(
        z2+(3+adj1),
        0,
        -z2-(3+adj1),
        0
      ),
      0:3,
      family="Times New Roman",
      font=2)
    
    ### The points
    Cos = cos(2*pi*((-dat1$bi.c%%1)+.25))
    Sin = sin(2*pi*((-dat1$bi.c%%1)+.25))
    Magn = .25+dat1$bi.c
    points(x = Cos*Magn,
           y = Sin*Magn,
           pch=(dat1$pos%%4)+15
    )
    
    # Draw legend
    legend.x.adjustment = 2 # use this to bump the legend to the side.
    legend(z2+legend.x.adjustment,z2+5,0:3,pch=15:18,text.font=3,bty="n")
    title(main=instrum,font.main=3,adj=0)
  })
}
# To produce Example 8.10, run the function defined above and add a plot
# of the rapping of Groove 1.
PlotLoop("getBy")
PlotVerseSpiral("getby1",row.index = 53:67,
                print.phase.adj = F,
                print.swing.adj = F,
                print.tempo.adj = F,
                width=3.75,launch.quartz = F,print.mean.nonalignment = F)
title(main="Kweli",font.main=3,adj=0)
quartz.save(file="Examples/Chapter 8/Example 8.10.pdf",type="pdf");dev.off()
