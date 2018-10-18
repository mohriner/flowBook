# If one needs to change the rhythm in a source file, or add an accent assertion
# to assertedAccents.txt, or change the first onset, run this script.
# 
# The script is mostly written to accomodate multiple verses in one call,
# but it's best to send them one at a time.
#setwd("/Users/mohriner/Dropbox/Research/RapWork/flowBook")
source("Scripts/loadLibraries.r")
load("DerivedData/FlowBookCorpus.rdata")
source("Scripts/PlottingScripts/PlotVerse.r")

vowels = c("AA","AE","AH","AO","AW","AY","EH","ER","EY","IH","IY","OW","OY","UH","UW")
vowels = sort(c(paste(vowels,"0",sep=""),paste(vowels,"1",sep=""),paste(vowels,"2",sep="")))
dict_hyph = read.delim("SourceData/PhoneticData/hyph_dict.txt",sep=" ",stringsAsFactors=F)
time_dat = read.delim("SourceData/verseAcapellaMetadata.txt",header=T)


reviseCorpus = function(revise,revise.bi.c=F){
  artists = laply(revise,function(i) strsplit(i,"_")[[1]][1])
  titles = laply(revise,function(i) strsplit(i,"_")[[1]][2])
  
  # Read in source data -----------------------------------------------------
  artistTitles = laply(1:length(artists),function(i){
    paste(artists[i],titles[i],sep="_")
  })
  remove("artists")
  setwd("SourceData/VerseTranscriptions/")
  # paste(titles,".txt",sep="") %in% list.files() # Make sure the files are there.
  source.transcriptions = llply(
    paste(artistTitles,".txt",sep=""),
    function(i) read.delim(i,header=T))
  setwd("../..")
  
  # Append CMU_accent -------------------------------------------------------
  # dat = source.transcriptions[[1]]
  source("Scripts/DataScrubbingScripts/AppendCMU.Accent.r")
  transc = llply(source.transcriptions,AppendCMU.Accent,.inform=T)
  names(transc) = titles
  
  # Append Beat Index -------------------------------------------------------
  # Get first beat from metadata file
  meta = read.delim("SourceData/verseMetadata.txt")
  o1 = laply(artistTitles,function(i){
    index = i %>% strsplit(.,"_") %>% .[[1]] %>% .[[2]] %>% match(.,meta$title)
    m1 = meta %>% slice(index) %>% .[["m1"]]
    b1 = meta %>% slice(index) %>% .[["b1"]]
    m1*4 + b1
  },.inform=T)
  
  # Compute beatIndex and append
  for(i in 1:length(titles)){
    dat = transc[[i]]
    n = dim(dat)[1]
    durations = (4/dat$quant) * dat$duration
    beatIndex = c(0,cumsum(durations) [1:(n-1)]) + o1[i] %>% round(.,digits=3)
    dat = cbind(dat,beatIndex)
    transc[[i]] = dat
  }
  #remove(list = c("durations","beatIndex","n","dat","i","o1","artistTitles"))
  
  # Append accent -----------------------------------------------------------
  
  source("Scripts/DataScrubbingScripts/AppendAccent.r")
  transc = llply(transc,AppendAccent)
  
  # Override accents manually -----------------------------------------------
  assertedAccents = read.delim("SourceData/AssertedAccents.txt",header=T)
  
  for(i in 1:length(transc)){
    targetVerse = strsplit(revise,"_")[[1]][2]
    dat = transc[[i]]
    dat$beatIndex = round(dat$beatIndex,digits=2)
    dat = cbind(dat,toggled = rep(F,length(dat[,1])))
    bi = assertedAccents %>% 
      mutate(beatIndex = ((measure-1)*4) + ((position/cardinality)*4)) %>% 
      filter(verse==targetVerse) %>% 
      .[["beatIndex"]] %>% sort %>% unique %>% round(.,digits=2)
    bi = bi[bi %in% (round(dat$beatIndex,digits=2))]
    dat$accent[which(dat$beatIndex %in% bi)] = !dat$accent[which(dat$beatIndex %in% bi)]
    dat$toggled[which(dat$beatIndex %in% bi)] = TRUE
    transc[[i]] = dat
  }
  remove(list = c("assertedAccents","bi","dat","i","targetVerse"))
  
  
  # Melt into single data frame ---------------------------------------------
  
  verses = titles
  corpora.addendum = melt(unname(transc),id.vars = 1:dim(transc[[1]])[2])
  # Append verse, artist, and corpus
  meta = read.delim("SourceData/verseMetadata.txt")
  corpora.addendum = cbind(corpora.addendum,verse = verses[corpora.addendum$L1])
  corpora.addendum = cbind(corpora.addendum,
                           artist = meta$artist[match(corpora.addendum$verse,meta$title)],
                           corpus = meta$tranche[match(corpora.addendum$verse,meta$title)])
  corpora.addendum = tbl_df(corpora.addendum)
  
  corpora.addendum = corpora.addendum %>% select(-L1)
  remove(list=c("verses","meta","transc"))
  
  # Append indexes of lines and phrases -------------------------------------
  source("Scripts/DataScrubbingScripts/IndexLinesAndPhrases.R")
  corpora.addendum = IndexLinesAndPhrases(corpora.addendum)
  
  
  # Append bi.c -------------------------------------------------------------
  
  if(!("bi.c" %in% names(corpora.addendum))){
    corpora.addendum = cbind(corpora.addendum[,1:8],
                             bi.c = rep(NA,dim(corpora.addendum)[1]),
                             corpora.addendum[,9:17])
  }
  if(revise.bi.c==T){
    # Insert bi.c if necessary if available and requested.
    setwd("SourceData/PhoneticData/flowBeatAlignment/CorrectedTextGrids/")
    f = list.files(pattern=revise)
    setwd("../../../..")
    if(length(f)>0){
      source("Scripts/DataScrubbingScripts/TextGrid2ContinuousOnset.r")
      setwd("SourceData/PhoneticData/flowBeatAlignment/CorrectedTextGrids/")
      TG = readLines(f)
      artist = strsplit(revise,"_")[[1]][1]
      verse = strsplit(revise,"_")[[1]][2]
      corpora.addendum$bi.c = TextGrid2ContinuousOnset(TG,artist,verse) %>% .$beatIndex.c %>% unlist
      setwd("../../../..")
    }
  }
  
  # Add to existing corpus --------------
  load("DerivedData/FlowBookCorpus.rdata")
  corpora = corpora %>% filter(!is.element(verse,titles))
  remove("titles")
  corpora = rbind(corpora,corpora.addendum)
  corpora$bi.c = unlist(corpora$bi.c)
  remove(list="corpora.addendum")
  corpora = tbl_df(corpora)
  corpora = corpora %>% arrange(corpus,artist,verse,beatIndex)
  save(list="corpora",file="DerivedData/FlowBookCorpus.rdata")
  write.table(corpora,"DerivedData/FlowBookCorpus.txt",quote=F,sep="\t",row.names=F)
}
#
#revise = "bigBoi_shutterbugg"
#reviseCorpus(revise)
#load("DerivedData/FlowBookCorpus.rdata")
PlotVerse("shutterbugg",plot.rhymeClasses = T,showOverride = T,
          m.range=NA,launch.quartz = T)
#corpora %>% filter(verse=="getBy1")
#corpora$verse %>% unique %>% sort


#getwd()
#setwd("../..")
