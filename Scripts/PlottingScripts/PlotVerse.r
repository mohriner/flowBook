source("Scripts/loadLibraries.r")
source("Scripts/DataScrubbingScripts/GetL3Durations.R")

# verseTitle = "aquemini2"
# plot.accent = T
# continuous = F
# combined = T
# small.only = T
# accentOnly = F
# revertToggles = F
# plot.rhymeClasses = T
# showOverride=T
# plot.phrase = T
# m.range = NA
# row.index=NA
# beatIndex.range = NA
# closeScreen = F
# plot.3limit = F
# m.numbers = NA
# Width=4.5
# meas.scale = 1
# displacement = 0
# plot.lyrics = T
# save.path = NA
# save.plot = F

PlotVerse = function(
  verseTitle,
  plot.accent = T, # Give accented syllables a larger point
  continuous = F, # Plot un-quantized onsets (only available for some verses)
  combined = F, # Plot both quantized and unquantized onsets (ditto)
  small.only = F, # Make every point small
  accentOnly = F, # Plot only accented syllables
  displacement = 0, # Adjust beat indexes
  revertToggles = F, # Plot output from accent detection algorithm, no manual corrections
  plot.rhymeClasses = F,
  showOverride=T, # Show manual override
  plot.phrase = T, # Use commas at ends of phrases
  m.range = NA, # Select measures for plotting
  row.index=NA, # Select rows (syllables) for plotting
  beatIndex.range = NA, # Select beats for plotting
  closeScreen = F, # Close screen after plotting
  plot.3limit = F, # Mark limit-3 accents with "+"
  m.numbers = NA, # Print measure numbers
  Width=4.5, # Plot width in inches
  meas.scale = 1, # Relative plot height 
  plot.lyrics = T, # Show lyrics
  launch.quartz = T, # Make new plot?
  save.path = NA, # File path for save
  save.plot = F # Save?
)
{
  if(combined==T){
    plot.rhymeClasses=F
    plot.3limit=F
    meas.scale = 1.25
  }
  
  ###---Initial data processing----
  dat = corpora %>% filter(verse==verseTitle)
  # Change beat index if asked
  dat$beatIndex = dat$beatIndex + displacement
  dat = dat %>% mutate(q.measure = beatIndex %/% 4,q.beat = beatIndex %% 4)
  
  # Revert toggles if asked. (This is mainly didactic, to show how the algorithm
  # behaves without any manually overrides)
  if(revertToggles==T){
    dat$accent[which(dat$toggled)] = dat$accent[which(dat$toggled)] %>% 
      as.logical %>% not %>% as.numeric
  }
  
  # Filter by row if there's a row.index argument
  if(!is.na(row.index[1])){
    dat = dat[row.index,]
  }
  
  # Remove measures not in m.range if m.range is specified
  if(!is.na(m.range[1])){
    dat = dat[which(is.element(unlist(dat[,"q.measure"]),m.range)),]
  }
  
  # Filter by beatIndex if asked
  if(!is.na(beatIndex.range[1])){
    beatIndex.range = range(beatIndex.range)
    dat = dat %>% filter(beatIndex>=beatIndex.range[1], beatIndex<=beatIndex.range[2])
  }
  
  n.meas = diff(range(dat[,"q.measure"]))+1
  meas.mult = .65
  
  # Remove unaccented syllables if requested
  if(accentOnly==T){
    dat = filter(dat,accent==1)
  }
  
  # Determine measure width from how much is getting plotted around it
  ###---Initialize plot ----
  n.features = plot.rhymeClasses + plot.3limit
  Meas.height = (.45 + (.125*n.features)) * meas.scale
  if(launch.quartz==T){
    quartz(width = Width,height = n.meas*Meas.height)
    par(mar = c(0,0,0,0))
  }
  
  plot(0,0,col="white",xlim=c(-.15,4),
       ylim=rev(range(dat[,"q.measure"])+c(-1,.5)),yaxp=c(n.meas,0,n.meas),
       xaxt="n",bty="n",las=1,mgp=c(1,0,0),tcl=.01,family="Times New Roman",font=3,
       yaxt="n")
  x = llply(max(dat$q.measure):min(dat$q.measure),function(x) lines(c(0,4),rep(x,2),lwd=.5))	
  remove(list=c("x"))
  ###---Layer 1: the meter grid ----
  # Draw the meter: not a graph-paper-like grid
  # get points
  n.meas = length(seq(min(dat$q.measure),max(dat$q.measure)))
  xs = rep(seq(0,3.75,length.out=16),each=2)
  ys = rep(0,34)
  ys[seq(1,33,2)] = c(rep(c(-.25,-.12,-.18,-.12),4),-.25);ys = ys[1:32]
  xs2 = rep(NA,17*3)
  xs2[seq(1,16*3+1,by=3)] = xs[seq(1,16*2+1,by=2)]
  xs2[seq(2,16*3+2,by=3)] = xs[seq(2,16*2+2,by=2)]
  ys2 = rep(NA,17*3)
  ys2[seq(1,16*3+1,by=3)] = ys[seq(1,16*2+1,by=2)]
  ys2[seq(2,16*3+2,by=3)] = ys[seq(2,16*2+2,by=2)]
  xs3 = rep(xs2,n.meas)
  ys3 = ys2+rep(seq(min(dat$q.measure),max(dat$q.measure)),each=length(ys2))
  line_coords = list(x=xs3,y=ys3)
  # The little dots at the top
  xs = seq(0,4,length.out=17)[1:16]
  ys= c(rep(c(-.25,-.12,-.18,-.12),4),-.25)[1:16]
  ys2 = rep(seq(min(dat$q.measure),max(dat$q.measure)),each=length(ys)) + rep(ys,length(seq(min(dat$q.measure),max(dat$q.measure))))
  ys = ys
  xs2 = rep(xs,n.meas)
  point_coords = list(x = xs2,y=ys2)
  lines(line_coords,lwd=.5)
  points(point_coords,pch=20,cex=.25)
  
  ###---Layer 2: The syllable points ----
  # Point size, determined by accent, cex= .5 or 1.25
  p.cex = rep(.5,length(unlist(dat[,1])));
  if(plot.accent==T){p.cex[which(dat[,"accent"]==1)] = 1.25}
  if(plot.accent==F){
    if(small.only==T){
      p.cex = rep(.5,length(unlist(dat[,1])));
    }else{
      p.cex = rep(1.25,length(unlist(dat[,1])));
    }
  }
  
  # Point shape, which can change to show manual overrides
  p.pch = rep(21,dim(dat)[1])
  p.pch[dat$toggled==1] = 24
  if(showOverride==FALSE){p.pch = rep(21,dim(dat)[1])}
  if(combined==F){
    points(unlist(dat[,"q.beat"]),unlist(dat[,"q.measure"]),cex=p.cex,pch=p.pch,bg="white")
  }
  # Plot combined points
  if(combined == T){
    dat = dat %>% 
      mutate(c.meas = bi.c %/% 4,
             c.beat = bi.c %% 4)
    # draw lines between them
    llply(1:length(dat$c.beat), function(i){
      if(dat$c.meas[i]==dat$q.measure[i] & !is.na(dat$c.meas[i])){
        lines(c(dat$c.beat[i],dat$q.beat[i]),c(dat$c.meas[i]-.35,dat$q.measure[i]))
      }
      
    })
    points(x = dat$c.beat,
           y = dat$c.meas -.35,
           cex = p.cex,
           pch=p.pch,
           bg = "white"
    )
    points(unlist(dat[,"q.beat"]),unlist(dat[,"q.measure"]),cex=p.cex,pch=p.pch,bg="white")
    
  }
  
  ###---Draw the position labels
  text(seq(0,3.75,.25),rep(min(dat$q.measure)-.75,16),0:15,family="Times New Roman",font=2,cex=.65)
  
  ###---Layer 3: the syllables ----
  # Plot syllable text
  # Put commas on syls with breath endings
  syls.to.plot = as.character(unlist(dat[,"syllable"]))
  breathEndings = c(which(diff(dat$breathEnding)==1)+1,dim(dat)[1])
  if(plot.phrase==T){
    syls.to.plot[breathEndings]  = paste(syls.to.plot[breathEndings],",",sep="")
  }
  if(plot.lyrics==T){
    text(unlist(dat[,"q.beat"]),unlist(dat[,"q.measure"])-.05,syls.to.plot,pos=1,cex=.65,font=3,family="Times New Roman")
  }
  ###---Layer 4: rhyme classes or features ----
  # draw the lines first so the numbers are plotted over them
  if(plot.rhymeClasses==TRUE){
    dat$rhymeClass = as.numeric(dat$rhymeClass)
    dat$rhymeIndex = as.numeric(dat$rhymeIndex)
    if(any(!is.na(dat$rhymeClass))){
      n.rhymes = max(dat[,"rhymeClass"],na.rm=T)
      for(i in 1:n.rhymes){
        r = which(dat[,"rhymeClass"]==i)
        
        if(length(r)>0){
          l = max(dat[r,"rhymeIndex"]) # the limit, n of syls in the rhyme
          r.syls = dat[r,"rhymeIndex"] %>% unlist %>% unname
          t = rep("c",length(r))
          
          # First syllable is beginning
          t[1] = "b"
          
          # Last syllable is either solo or ending
          t[length(t)] = "e"; if(r.syls[length(r.syls %>% unlist)]==1){t[length(t)] = "s"}
          
          # Figure out the rest
          r2 = (length(r)-1)
          if(r2>0){
            for(j in 1:r2){
              if(all(r.syls[j+1]==1,r.syls[j] ==1 )){t[j] = "s"}else{
                if(sign(r.syls[j+1] - r.syls[j])==-1){t[j] = "e"}
              }
            }
          }
          if(length(r)>2){
            for(j in 2:(length(r)-1)){
              if(t[j] == "e"){t[j+1] = "b"}
            }
          }
          
          # Draw some lines
          if(is.element("b",t)){
            for(j in which(t=="b")){
              start1 = r[j] # index of first syllable of rhyme
              end1 = r[which(t=="e")[which(t=="e")>j][1]] # index of last syllable of rhyme
              if(is.na(end1)){end1 = start1}
              if(dat[start1,"q.measure"]==dat[end1,"q.measure"]){
                # The rhyme is contained within a measure
                lines(dat[c(start1,end1),"q.beat"] %>% unlist,dat[c(start1,end1),"q.measure"] %>% unlist-rep(.35,2),lwd = .5)
              }else{
                # Not the same measure
                lines(c(dat[start1,"q.beat"] %>% unlist,4.05),rep(dat[start1,"q.measure"] %>% unlist,2)-.35,lwd = .5)
                lines(c(dat[end1,"q.beat"] %>% unlist,-.1),rep(dat[end1,"q.measure"] %>% unlist,2)-.35,lwd = .5)
              }
            }
          }
        }
      }
      
      # Number them
      if(is.element("rhymeClass",colnames(dat))){
        r.syls = which(!is.na(dat[,"rhymeClass"]))
        points(dat[r.syls,"q.beat"] %>% unlist,dat[r.syls,"q.measure"] %>% unlist-.35,pch = 16,cex = 1.25)
        text(dat[r.syls,"q.beat"] %>% unlist,dat[r.syls,"q.measure"] %>% unlist-.35,dat[r.syls,"rhymeClass"] %>% unlist,col="white",cex=.45)
      }
    }else{
      warning("no rhyme classes to print")
    }
  }
  ###---Layer 5: Plot 3-limit durations ----
  if(plot.3limit==TRUE){
    l3.dur = GetL3Durations(verseTitle)
    l3.dur = strsplit(l3.dur,"")[[1]] %>% as.numeric() %>% cumsum %>% c(0,.)
    l3.dur.x = (l3.dur%%16)/4
    l3.dur.y = floor(l3.dur/16)
    points(l3.dur.x,l3.dur.y+.3,pch=3,cex=.5)
  }
  
  ###---Layer 6: Number measures----
  if(is.na(m.numbers[1])){
    for(i in unique(dat[,"q.measure"])){
      text(-.15,i,i+1,family="Times New Roman",font=3,cex = .75)
    }
  }else{
    for(i in unique(dat[,"q.measure"])){
      text(-.15,i,m.numbers[i+1],family="Times New Roman",font=3,cex = .75)
    }
  }
  
  # Save
  if(save.plot==T){
    quartz.save(file=save.path,type="pdf");
  }
  
  # Close the screen
  if(closeScreen==T){
    if(length(dev.list())>0){
      dev.off()
    }
  }
}
