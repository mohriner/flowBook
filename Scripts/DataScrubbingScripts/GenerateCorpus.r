# This script takes the source files of verse transcriptions and creates the corpus

# Read in source data -----------------------------------------------------

setwd("SourceData/VerseTranscriptions/")
f = list.files()
titles = unlist(strsplit(f,".txt"))
source.transcriptions = llply(f,function(i) read.delim(i,header=T),.progress="text")
setwd("../..")

# Append CMU_accent -------------------------------------------------------

source("Scripts/DataScrubbingScripts/AppendCMU.Accent.r")
transc = llply(source.transcriptions,AppendCMU.Accent,.inform=T,.progress="text")
names(transc) = titles
save(list="transc",file="DerivedData/transcriptions.with.CMU.accent.rdata")

# Append Beat Index -------------------------------------------------------
load("DerivedData/transcriptions.with.CMU.accent.rdata")
# Get first beat from metadata file
meta = read.delim("SourceData/verseMetadata.txt")
o1 = laply(titles,function(i){
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
# The next line adds blank "bi.c" columns to all the non-Kendrick verses
transc[which(laply(transc,length)==10)] = 
  llply(transc[which(laply(transc,length)==10)],function(dat){
    dat %>% mutate(bi.c = rep(NA,dim(dat)[1])) %>% 
      select(names(transc[[191]]))
  })
remove(list = c("durations","beatIndex","n","dat","i","o1"))
save(list="transc",file="DerivedData/transcriptions.before.accent.detection.rdata")

# Append accent -----------------------------------------------------------
load("DerivedData/transcriptions.before.accent.detection.rdata")

source("Scripts/DataScrubbingScripts/AppendAccent.r")
transc = llply(transc,AppendAccent,.progress="text",.inform=T)
save(list="transc",file="DerivedData/transcriptions.with.detectedAccent.rdata")

# Override accents manually -----------------------------------------------


setwd("SourceData/VerseTranscriptions/")
f = list.files()
titles = unlist(strsplit(f,".txt"))
setwd("../..")
load("DerivedData/transcriptions.with.detectedAccent.rdata")
assertedAccents = read.delim("SourceData/AssertedAccents.txt",header=T)

for(i in 1:length(transc)){
  targetVerse = laply(titles,function(j) strsplit(j,"_")[[1]][2])[i]
  dat = transc[[i]]
  dat$beatIndex = round(dat$beatIndex,digits=2)
  dat = cbind(dat,toggled = rep(F,length(dat[,1])))
  bi = assertedAccents %>% 
    mutate(beatIndex = ((measure-1)*4) + ((position/cardinality)*4)) %>% 
    filter(verse==targetVerse) %>% 
    .[["beatIndex"]] %>% sort %>% unique
  bi = bi[bi %in% (round(dat$beatIndex,digits=2))]
  dat$accent[which(dat$beatIndex %in% bi)] = !dat$accent[which(dat$beatIndex %in% bi)]
  dat$toggled[which(dat$beatIndex %in% bi)] = TRUE
  transc[[i]] = dat
}
remove(list = c("assertedAccents","bi","dat","i","targetVerse"))


# Melt into single data frame ---------------------------------------------

verses = laply(titles,function(j) strsplit(j,"_")[[1]][2])
artists = laply(titles,function(j) strsplit(j,"_")[[1]][2])
corpora = melt(unname(transc),id.vars = 1:dim(transc[[1]])[2])
# Append verse, artist, and corpus
meta = read.delim("SourceData/verseMetadata.txt")
corpora = cbind(corpora,verse = verses[corpora$L1])
corpora = cbind(corpora,
                artist = meta$artist[match(corpora$verse,meta$title)],
                corpus = meta$tranche[match(corpora$verse,meta$title)])
corpora = tbl_df(corpora)
corpora = corpora %>% select(-L1)
remove(list=c("verses","artists"))

# Append indexes of lines and phrases -------------------------------------
source("Scripts/DataScrubbingScripts/IndexLinesAndPhrases.R")
corpora = IndexLinesAndPhrases(corpora)

# Save --------------------------------------------------------------------

save(list="corpora",file="DerivedData/FlowBookCorpus.rdata")
write.table(corpora,"DerivedData/FlowBookCorpus.txt",quote=F,sep="\t",row.names=F)
remove(list=c("transc","IndexLinesAndPhrases"))
