source("Scripts/AnalysisScripts/NonAlignmentFunctions.r")
###---Data processing---###
process.dat = function(verseTitle,row.index,tempo.adj=1,phase.adj=0,swing.adj=1)
{
  dat = corpora %>% 
    filter(verse==verseTitle) %>% 
    mutate(c.meas = bi.c %/% 4,
           c.beat = bi.c %% 4,
           q.measure = beatIndex %/% 4,
           q.beat = beatIndex %% 4)
  
  # Get rid of most of the columns
  dat = dat %>% select(syllable,c.meas,c.beat,q.measure,q.beat,accent,toggled,beatIndex,bi.c)
  names(dat)[which(names(dat)=="beatIndex")] = "bi.q"
  names(dat)[which(names(dat)=="bi.c")] = "bi.c"
  # select only certain rows
  if(!all(is.na(row.index))){dat = dat[row.index,]}
  
}


# verseTitle = "getby1"
# m.range = NA
# row.index = 96:111
# quantized=FALSE
# phase.adj = 0 # in minutes
# tempo.adj = 1.01 # in percentage
# swing.adj = 1.3 # in upper number of swing ratio
# show.compass=FALSE
# show.five.min.marks = TRUE
# clock.number.displacement = 3
# n.measures = 3
# print.phase.adj = T
# print.swing.adj = T
# print.tempo.adj = T
# print.mean.nonalignment = T

###---The Plotting Function---###
PlotVerseSpiral = function( verseTitle,
                            m.range = NA,
                            row.index = NA,
                            quantized=FALSE,
                            phase.adj = 0, # in minutes
                            tempo.adj = 1, # in percentage
                            swing.adj = 1, # in upper number of swing ratio
                            show.compass=FALSE,
                            show.five.min.marks = TRUE,
                            clock.number.displacement = 1.5,
                            n.measures = 5,
                            print.phase.adj = T,
                            print.swing.adj = T,
                            print.tempo.adj = T,
                            print.mean.nonalignment = T,
                            width=2.25,
                            launch.quartz=T)
{
  dat = corpora %>% 
    filter(verse==verseTitle) %>% 
    mutate(c.meas = bi.c %/% 4,
           c.beat = bi.c %% 4,
           q.measure = beatIndex %/% 4,
           q.beat = beatIndex %% 4) %>% 
    select(syllable,c.meas,c.beat,q.measure,q.beat,
           accent,toggled,beatIndex,bi.c)
  # select only certain rows
  if(!all(is.na(row.index))){dat = dat[row.index,]}
  
  
  # Rename columns
  names(dat)[which(names(dat)=="beatIndex")] = "bi.q"
  names(dat)[which(names(dat)=="bi.c")] = "bi.c"
  
  # adjust to start with measure 1 (the origin of the spiral)
  m1 = min(floor(dat[,c("bi.q","bi.c")]),na.rm=T) - 1
  dat$bi.q = dat$bi.q - m1
  dat$bi.c = dat$bi.c - m1
  
  # Adjust onsets
  dat$bi.c = AdjustContinuousOnsets(dat$bi.q,dat$bi.c,phase.adj,swing.adj,tempo.adj)
  
  ###--- Plotting ---###
  if(launch.quartz==T){
    quartz(width=width,height=width)
    par(mar=c(0,0,0,0),cex = .65,las=1,bty='n',
        family="Times New Roman")
  }
  # Define x and y limits, number of beats
  z1 = min(ceiling(dat$bi.c),na.rm=T)-1 # for xlim and ylim
  z2 = max(ceiling(dat$bi.c),na.rm=T) # for xlim and ylim
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
    if(show.five.min.marks==TRUE){
      lines(cos(c(pi*i,pi*(i+1)))*(z2+1),
            sin(c(pi*i,pi*(i+1)))*(z2+1),
            lwd=.25)
    }
    text(cos(c(pi*i,pi*(i+1)))*(z2+clock.number.displacement),
         sin(c(pi*i,pi*(i+1)))*(z2+clock.number.displacement),
         labels = paste(":",Min[,h],sep=""))
    
  }
  
  # plot compass
  if(show.compass==TRUE){
    lines(cos(c(pi*.25,pi*(.25+1)))*(z2+1),
          sin(c(pi*.25,pi*(.25+1)))*(z2+1),
          lwd=1,
          lty=2)
    lines(cos(c(pi*.75,pi*(.75+1)))*(z2+1),
          sin(c(pi*.75,pi*(.75+1)))*(z2+1),
          lwd=1,
          lty=2)
  }
  
  # The spiral
  lines(x = rep(cos(-seq(0,2*pi,length.out=1000)+(pi/2)),n)*(seq(z1,z2,length.out=1000*n)+.25),
        y = rep(sin(-seq(0,2*pi,length.out=1000)+(pi/2)),n)*(seq(z1,z2,length.out=1000*n)+.25),
        lwd=.25
  )
  
  ### Divisions of the beat
  adj1 = 1.25
  text(
    x = c(
      0,
      z2+(clock.number.displacement+adj1),
      0,
      -z2-(clock.number.displacement+adj1)
    ),
    y = c(
      z2+(clock.number.displacement+adj1),
      0,
      -z2-(clock.number.displacement+adj1),
      0
    ),
    0:3,
    family="Times New Roman",
    font=2)
  
  ### The points
  if(quantized==TRUE){
    dat$bi.c = dat$bi.q
  }
  Cos = cos(2*pi*((-dat$bi.c%%1)+.25))
  Sin = sin(2*pi*((-dat$bi.c%%1)+.25))
  Magn = .25+dat$bi.c
  points(x = Cos*Magn,
         y = Sin*Magn,
         pch=((dat$q.beat%%1)*4)+15
  )
  
  ### The text of lyrics
  pos = 1;if(quantized==T){pos=4}
  text(x = Cos*Magn,
       y = Sin*Magn,
       labels = dat$syllable,
       family="Times New Roman",
       font=4,pos=pos)
  
  # Draw legend
  legend.x.adjustment = 2 # use this to bump the legend to the side.
  legend(z2+legend.x.adjustment,z2+5,0:3,pch=15:18,text.font=3,bty="n")
  
  # Print the adjustment values
  
  t1 = paste("phase: ",phase.adj," min;",sep="")
  t2 = paste("swing: ",swing.adj,":1;",sep="")
  t3 = paste("tempo: ",round((1-tempo.adj)*100,digits=1),"%.",sep="")
  sub1 = ""
  if( print.phase.adj ){sub1 = paste(sub1,t1,sep=" ")}
  if( print.swing.adj ){sub1 = paste(sub1,t2,sep=" ")}
  if( print.tempo.adj ){sub1 = paste(sub1,t3,sep=" ")}
  title(sub=sub1,line=-1,font.sub=3,family="Times New Roman")
  
  mean.async = round(60 * mean(abs(dat$bi.c - dat$bi.q),na.rm=T),digits=2)
  if(print.mean.nonalignment){
    text(-(z2+(clock.number.displacement+2.5)),(z2+(clock.number.displacement+2.5)),
         paste("Non-alignment: ",mean.async," min",sep=""),pos = 4, font = 3)
  }
}

###--- A function to add linear regression lines to the plot ---###
draw.line.from.origin = function(data.path,piece.path,row.index,minute){
  dat = process.dat(data.path,piece.path,row.index)
  x = sin(2 * pi * (minute/60))
  y = cos(2 * pi * (minute/60))
  Magn = .25+dat$bi.c
  lines(c(x*.25,x*Magn),c(y*.25,y*Magn))
}

# ###--- Run the functions ---###
# plotVerse_spiral(data.path = "Data/verse data/",
#                  piece.path = "talibKweli_getBy1",
#                  m.range = NA,
#                  #row.index = c(175:199),
#                  #row.index = c(143:196),
#                  #row.index = 212:243,
#                  row.index = 96:111,
#                  quantized=F,
#                  phase.adj = -1.5,
#                  tempo.adj = 1.01,
#                  swing.adj = 1.3,
#                  show.compass=T,
#                  show.five.min.marks = F,
#                  clock.number.displacement = 1.5,
#                  n.measures = 2
# )

# Add lines for "Get By" hook
# for(i in c(47.4,16.8,28.2,58.2)+1.8){ # +1.8 for the adjustment
#   draw.line.from.origin(
#     data.path = "Data/verse data/",
#     piece.path = "talibKweli_getBy1",
#     row.index = c(96:111),
#     minute = i
#   )
# }