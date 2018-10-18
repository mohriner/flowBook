source("Scripts/loadLibraries.r")
load("DerivedData/FlowBookCorpus.rdata")

# A function that plots a verse on the metric grid ------------------------

quickPlot = function(verse, new=T,m.range = NA){
  onset.data = read.delim("SourceData/verseMetadata.txt",header=T)
  path = paste("SourceData/VerseTranscriptions/",verse,".txt",sep="")
  dat = read.delim(path,header=T)
  index = which(paste(onset.data$artist,onset.data$title,sep="_")==verse)
  m1 = onset.data$m1[index]
  b1 = onset.data$b1[index]
  dur = (4/dat$quant) * dat$duration
  beat.index = c(0,cumsum(dur)) %>% .[1:((length(.)-1))]
  beat.index = beat.index + ( m1*4 + b1)
  dat = mutate(dat,beat.index)
  if(is.na(m.range[1])){
    m.range = c(min(floor(beat.index/4)),max(ceiling(beat.index/4)))
  }else{
    m.range = range(m.range)
  }
  n.meas = diff(m.range)
  if(new==T){quartz(width=4.55,height=n.meas*.5)}
  par(mar=c(0,0,0,0))
  plot(beat.index %% 4, beat.index %/% 4, ylim=rev(m.range),
       ylab="",xlab="beat",yaxt="n",bty="n",pch=20,xlim=c(0,4))
  for(i in m.range[1]:m.range[2]){lines(c(0,4),rep(i,2))}
  text(beat.index %% 4, beat.index %/% 4,dat$syllable,pos=1,font=3,cex=.75)
  for(i in 0:3){lines(rep(i,2),m.range)}
}


# Once rhythm and rhyme is done, move onto "Revise Corpus.r"
#quartz()
quickPlot("arrestedDevelopment_living2",
          new=F,
          m.range = 0:10)
  #
