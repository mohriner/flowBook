# This script applies to the Kendrick Lamar data in the corpus, which was added
# after the completion of the manuscript in preparation for the "Corpus Studies
# of Hip Hop" chapter in the Oxford Handbook of Musical Corpus Studies.


source("Scripts/loadLibraries.r")
source("Scripts/DataScrubbingScripts/TextGrid2ContinuousOnset.r")

vowels = c("AA","AE","AH","AO","AW","AY","EH","ER","EY","IH","IY","OW","OY","UH","UW")
vowels = sort(c(paste(vowels,"0",sep=""),paste(vowels,"1",sep=""),paste(vowels,"2",sep="")))
dict_hyph = read.delim("SourceData/PhoneticData/hyph_dict.txt",sep=" ",stringsAsFactors=F)
time_dat = read.delim("SourceData/verseAcapellaMetadata.txt",header=T)

# Get verses with acapellas
setwd("SourceData/PhoneticData/flowBeatAlignment/CorrectedTextGrids/")
f = list.files(pattern = "kendrickLamar")

# The main function
TextGrid2ContinuousOnset = function(TG,artist,verse){
  a = artist
  v = verse
  s1 = grep("item \\[1\\]",TG)
  s2 = grep("item \\[2\\]",TG)
  
  # Lines with bracketed numbers
  ints = grep("\\[[[:digit:]]",TG)
  l1 = ints[which(ints>s1&ints<s2)]
  l2 = ints[which(ints>s2)]
  on1 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+1]))
  off1 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+2]))
  lab1 = gsub("[(text\\=[:space:]\")]","",TG[l1+3])
  on2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+1]))
  off2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+2]))
  lab2 = gsub("[(text\\=[:space:]\")]","",TG[l2+3])
  
  ph.matrix = data.frame(on1,off1,lab1)
  w.matrix = data.frame(on2,off2,lab2)
  # Abbreviate the two matrices
  ph.matrix = ph.matrix[-which(ph.matrix$lab1=="sp"),]
  w.matrix = w.matrix[-which(w.matrix$lab2=="sp"),]
  if(any(w.matrix$lab2=="")){w.matrix = w.matrix[-which(w.matrix$lab2==""),]}
  
  # Get continuous onset
  onset = vector()
  words = vector()
  for(j in 1:length(w.matrix$lab2)){	# for each word
    lab = w.matrix$lab2[j] # text of word
    
    # Determine phone boundaries
    sW = as.vector(unlist(w.matrix[j,1]))	# word start (s)
    eW = as.vector(unlist(w.matrix[j,2]))	# word end (s)
    sP = which.min(abs(ph.matrix[,1]-sW))	# phone start (index)
    eP = which.min(abs(ph.matrix[,2]-eW))	# phone end (index)
    if(j==length(w.matrix$lab2)){eP = length(ph.matrix[,1])}
    
    phones = as.vector(unlist(ph.matrix[sP:eP,"lab1"]))
    v1 = which(is.element(phones,vowels))	# index of phones that are vowels
    onset = c(onset,ph.matrix$on1[(sP:eP)[v1]])
    words = c(words,rep(as.vector(unlist(w.matrix$lab2[j])),length(v1)))
  }
  # Mod to measure, accounting for drift. To do this, you need to change the 
  # duration of the measure by the percentage discovered in measuring drift.
  i = match(verse,time_dat$title)
  dbA = time_dat$dbA[i]
  meas = 240/time_dat$tempo[i]
  
  d1 = time_dat$diff1[i]
  d2 = time_dat$diff2[i]
  m1 = time_dat$mixRef1[i]
  m2 = time_dat$mixRef2[i]
  a1 = time_dat$accRef1[i]
  a2 = time_dat$accRef2[i]
  dbA = ((time_dat$dbAmeas[i])*meas)

  onset1 = onset + (m1-a1) - dbA
  c.meas = floor(onset1/meas)
  c.beat = ((onset1%%meas)/meas)*4
  beatIndex.c = c.meas * 4 + c.beat
  data.frame(word=words,beatIndex.c = beatIndex.c)
}

# Read in TextGrids, convert to continuous onsets
bi.cont = llply(f,function(i){
  artist = strsplit(i,"[\\.\\_]")[[1]][1]
  verse = strsplit(i,"[\\.\\_]")[[1]][2]
  TG = readLines(i)
  TextGrid2ContinuousOnset(TG,artist,verse)
},.inform=T,.progress = "text")
setwd("../../../../")
names(bi.cont) = f

# Append continuous onsets. Note: dimensions might be different. Will be 
# annoying if they are.
junk = llply(1:length(f),function(i){
  dat1 = read.delim2(paste("SourceData/VerseTranscriptions/",
                          gsub("TextGrid","txt",f[i]),sep=""))
  dat2 = bi.cont[[i]]
  n1 = dim(dat1)[1]
  n2 = dim(dat2)[1]
  names(dat2) = c("word.TG","bi.c")
  if(n1>n2){ # original matrix is bigger than TextGrid-derived matrix
    dat2.new = data.frame(word.TG = rep(NA,n1-n2),bi.c = rep(NA,n1-n2))
    dat2 = rbind(dat2,dat2.new)
  }
  if(n1<n2){
    dat1.new = data.frame(word = rep(NA,n2-n1),
                          syllable = rep(NA,n2-n1),
                          quant = rep(NA,n2-n1),
                          duration = rep(NA,n2-n1),
                          rhymeClass = rep(NA,n2-n1),
                          rhymeIndex = rep(NA,n2-n1),
                          lineEnding = rep(NA,n2-n1),
                          breathEnding = rep(NA,n2-n1)
                          )
    dat1 = rbind(dat1,dat1.new)
  }
  dat3 = cbind(dat1,dat2)
  dat3 = dat3[,c(10,9,1:8)]
  write.table(dat1,
              paste("SourceData/VerseTranscriptions/",gsub("\\.TextGrid","",f[i]),"_old.txt",sep=""),
              quote=F,row.names = F,sep="\t")
  write.table(dat3,
              paste("SourceData/VerseTranscriptions/",gsub("\\.TextGrid","",f[i]),".txt",sep=""),
              quote=F,row.names = F,sep="\t")
})

# Perform a really naive quantization
setwd("SourceData/VerseTranscriptions/")
f = list.files(pattern = "kendrickLamar")
for(i in 1:30){
  read.delim2(f[i],header=T,stringsAsFactors = F) %>% tbl_df %>% 
    mutate(bi.q = round(as.numeric(bi.c)*4)/4) %>% 
    mutate(dur1 = c(diff(bi.q),NA)) -> dat
  dat$duration = dat$dur1/.25
  dat %>% select(word,syllable,quant,duration,rhymeClass,
                 rhymeIndex,lineEnding,breathEnding,bi.c) %>% 
    write.table(.,f[i],sep="\t",quote=F,row.names=F)
}
