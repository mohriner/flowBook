source("Scripts/loadLibraries.r")
load("DerivedData/FlowBookCorpus.rdata")


# A function that plots a verse on the metric grid ------------------------


quickPlot.continuous = function(verse, new=T,m.range = NA,c.bi.adjust = 0){
  onset.data = read.delim("SourceData/verseMetadata.txt",header=T)
  path = paste("SourceData/VerseTranscriptions/",verse,".txt",sep="")
  dat = read.delim(path,header=T)
  names(dat)[9] = "bi.c"
  index = which(paste(onset.data$artist,onset.data$title,sep="_")==verse)
  m1 = onset.data$m1[index]
  b1 = onset.data$b1[index]
  dur = (4/dat$quant) * dat$duration
  bi.q = c(0,cumsum(dur)) %>% .[1:((length(.)-1))]
  bi.q = bi.q + ( m1*4 + b1)
  bi.c = dat$bi.c + c.bi.adjust
  dat = mutate(dat,bi.q)
  if(is.na(m.range[1])){
    m.range = c(min(floor(bi.c/4)),max(ceiling(bi.c/4)))
  }else{
    m.range = range(m.range)
  }
  n.meas = diff(m.range)
  if(new==T){quartz(width=4.55,height=n.meas*.5)}
  par(mar=c(0,0,0,0))
  plot(bi.c %% 4, bi.c %/% 4, ylim=rev(m.range),
       ylab="",xlab="beat",yaxt="n",bty="n",pch=20,xlim=c(0,4))
  for(i in m.range[1]:m.range[2]){lines(c(0,4),rep(i,2),lwd=.25)}
  text(bi.c %% 4, bi.c %/% 4,dat$syllable,pos=1,font=3,cex=.75,family="Times New Roman")
  for(i in 0:3){lines(rep(i,2),m.range,lwd=.25)}
  
}

quickPlot = function(verse, new=T,m.range = NA,c.bi.adjust = 0){
  onset.data = read.delim("SourceData/verseMetadata.txt",header=T)
  path = paste("SourceData/VerseTranscriptions/",verse,".txt",sep="")
  dat = read.delim(path,header=T)
  names(dat)[9] = "bi.c"
  index = which(paste(onset.data$artist,onset.data$title,sep="_")==verse)
  m1 = onset.data$m1[index]
  b1 = onset.data$b1[index]
  dur = (4/dat$quant) * dat$duration
  bi.q = c(0,cumsum(dur)) %>% .[1:((length(.)-1))]
  bi.q = bi.q + ( m1*4 + b1)
  bi.c = dat$bi.c + c.bi.adjust
  dat = mutate(dat,bi.q)
  if(is.na(m.range[1])){
    m.range = c(min(floor(bi.q/4)),max(ceiling(bi.q/4)))
  }else{
    m.range = range(m.range)
  }
  n.meas = diff(m.range)
  if(new==T){quartz(width=4.55,height=n.meas*.5)}
  par(mar=c(0,2,0,0))
  plot(bi.q %% 4, bi.q %/% 4, ylim=rev(m.range),
       ylab="",xlab="beat",bty="n",pch=20,xlim=c(0,4),las=1)
  for(i in m.range[1]:m.range[2]){lines(c(0,4),rep(i,2),lwd=.25)}
  text(bi.q %% 4, bi.q %/% 4,dat$syllable,pos=1,font=3,cex=.75,family="Times New Roman")
  points(bi.c %% 4, (bi.c %/% 4)-.25,pch=20)
  for(i in 0:3){lines(rep(i,2),m.range,lwd=.25)}
  for(i in 1:length(bi.c)){
    if((bi.c[i] %/% 4) == (bi.q[i] %/% 4)){
      lines(c(bi.c[i]%% 4,bi.q[i]%% 4),c((bi.c[i] %/% 4)-.25,bi.q[i] %/% 4),lwd=.5)
    }
  }
}


# Once rhythm and rhyme is done, move onto "Revise Corpus.r"
verses = list.files("SourceData/VerseTranscriptions",pattern="arrested") %>% 
  gsub("\\.txt","",.)
i = 1
print(verses[i])
quickPlot(verses[i],new=F,
          m.range = 12:20)
quickPlot.continuous(verses[i])
