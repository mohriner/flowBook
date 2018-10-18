# This script creates the audio excerpts from the book, drawn from mp3 files
# of the songs that are stored on the author's local machine. Also creates
# 1-second fade in and out, and normalizes
source("Scripts/loadLibraries.r")
ex = read_xlsx("Back Matter.xlsx",sheet="Audio Excerpts")
ex = filter(ex,is.element(Chapter,c(8)))

skip = c("lupeFiasco_theShowGoesOn")
skipped = vector()
for(i in 1:dim(ex)[1]){
  path = paste("/Users/mohriner/Dropbox/Research/In Review/RapWork/Audio/complete tracks/",
               ex$fileName[i],".mp3",sep="")
  track = readMP3(path)
  sr = track@samp.rate
  bits = track@bit
  s = ex$start[i]-1
  e = ex$stop[i]+1
  samps.l = track@left[(s*sr):(e*sr)]
  samps.l[1:sr] = samps.l[1:sr]* seq(0,1,length.out = sr)
  samps.l[(length(samps.l)-sr): (length(samps.l))] = 
    samps.l[(length(samps.l)-sr): (length(samps.l))] * seq(1,0,length.out = sr+1)
  samps.r = track@right[(s*track@samp.rate):(e*track@samp.rate)]
  samps.r[1:sr] = samps.r[1:sr]* seq(0,1,length.out = sr)
  samps.r[(length(samps.r)-sr): (length(samps.r))] = 
    samps.r[(length(samps.r)-sr): (length(samps.r))] * seq(1,0,length.out = sr+1)
  new.ex = Wave(left = samps.l,right=samps.r,samp.rate = sr,bit = bits)
  new.ex = normalize(new.ex,unit = "16")
  Ch = ex$Chapter[i]
  Nu = ex$Number[i]
  Le = ex$Letter[i]
  if(is.na(Le)){Le = ""}else{if(Le=="NA"){Le = ""}}
  fileName = paste("Excerpt",".",Ch,".",Nu,Le,", ",ex$Caption[i],".wav",sep="")
  if(! ex$fileName[i] %in% skip){
    writeWave(new.ex,paste("AudioExcerpts/Chapter ",Ch,"/",fileName,sep=""))
  }else{
    skipped = c(skipped,fileName)
  }
}
