source("/Users/mohriner/Dropbox/Research/RapWork/flowBook/Scripts/AnalysisScripts/nonAlignmentSearchGrid.r")
###--- Functions ---###
# This function alters the onsets of events in a continuous transcription by 
# their phase (in minutes, 60 minutes = 1 beat), swing ratio (so 1.7 means
# 1.7:1), and tempo (in percent, so 1 means no change). It returns a measurement
# of asynchrony, in minutes

AdjustContinuousOnsets = function(bi.q,bi.c,phase.adj,swing.adj,tempo.adj){
  # Apply swing
  off.beat = which(bi.q %% 1 %in% c(.25,.75))
  bi.c[off.beat] = bi.c[off.beat] - (((swing.adj/(swing.adj+1)) - .5)/2)
  
  # Apply phase
  bi.c = bi.c + (phase.adj/60)
  
  # Apply tempo shift. 
  bi.c2 = bi.c * (1/tempo.adj)
  bi.c2 = bi.c2 - (mean(bi.c2,na.rm=T) - mean(bi.c,na.rm=T))  
  bi.c = bi.c2
  bi.c
}

CalculateNonalignment = function(bi.q,bi.c,phase.adj,swing.adj,tempo.adj){
  bi.c = AdjustContinuousOnsets(bi.q,bi.c,phase.adj,swing.adj,tempo.adj)
  
  # New non-alignment. Because of substantial phase shifts, it might be better
  # to look at alignment with the beat before or after.
  non.alignment = sum(abs(bi.c - bi.q),na.rm=T)
  non.alignment = round((non.alignment/length(bi.c))*60,digits=2)
  return(non.alignment)
}

OptimizeAlignment = function(bi.q,
                              bi.c,
                              check.phase = T,
                              check.swing = T,
                              check.tempo = T)
{
  original = CalculateNonalignment(bi.q,bi.c,phase.adj=0,swing.adj = 1,tempo.adj = 1)
  m2 = m
  if(!check.phase){
    m2 = filter(m2,phase==0)
  }
  if(!check.swing){
    m2 = filter(m2,swing==1)
  }
  if(!check.tempo){
    m2 = filter(m2,tempo==1)
  }
  x = laply(1:length(m2[,1]),function(i){
    phase1 = m2$phase[i]
    swing1 = m2$swing[i]
    tempo1 = m2$tempo[i]
    CalculateNonalignment(bi.q,
                     bi.c,
                     phase.adj = phase1,
                     swing.adj = swing1,
                     tempo.adj = tempo1)
  }) # this takes about 3 seconds
  
  # Next, choose the adjustments that makes for the lowest asynchronies. If there
  # are ties, choose the adjustment that is closest to 'neutral', i.e., phase,
  # swing, and tempo are 0, 1, and 1.
  winners = which(x==min(x))
  l = m2[winners,]
  
  l2 = apply(t(l),2,function(i) i-c(0,1,1))
  l2[1,] = l2[1,] / 30
  l2[2,] = l2[2,] / 3.55
  l2[3,] = l2[3,] / 1.05
  winner = winners[which.min(apply(abs(l2),2,mean))]
  
  
  optimal = data.frame(phase = m2[winner,"phase"],
                       swing = m2[winner,"swing"],
                       tempo = m2[winner,"tempo"],
                       async1 = original,
                       async2 = x[winner],
                       expl = round((original-min(x))/original,digits=2)
  )
  optimal
}