# This script takes the source files of verse transcriptions and 
# adds them to the existing corpus.
source("Scripts/loadLibraries.r")

# Read in source data -----------------------------------------------------
titles = c("kurtisBlow_basketball")
setwd("SourceData/VerseTranscriptions/")
# paste(titles,".txt",sep="") %in% list.files() # Make sure the files are there.
source.transcriptions = llply(
  paste(titles,".txt",sep=""),
  function(i) read.delim(i,header=T),.progress="text")
setwd("../..")

# Append CMU_accent -------------------------------------------------------

source("Scripts/DataScrubbingScripts/AppendCMU.Accent.r")
transc = llply(source.transcriptions,AppendCMU.Accent,.inform=T,.progress="text")
names(transc) = titles

# Append Beat Index -------------------------------------------------------
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
remove(list = c("durations","beatIndex","n","dat","i","o1"))

# Append accent -----------------------------------------------------------

source("Scripts/DataScrubbingScripts/AppendAccent.r")
transc = llply(transc,AppendAccent,.progress="text")

# Override accents manually -----------------------------------------------
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
corpora.addendum = melt(unname(transc),id.vars = 1:dim(transc[[1]])[2])
# Append verse, artist, and corpus
meta = read.delim("SourceData/verseMetadata.txt")
corpora.addendum = cbind(corpora.addendum,verse = verses[corpora.addendum$L1])
corpora.addendum = cbind(corpora.addendum,
                artist = meta$artist[match(corpora.addendum$verse,meta$title)],
                corpus = meta$tranche[match(corpora.addendum$verse,meta$title)])
corpora.addendum = tbl_df(corpora.addendum)
corpora.addendum = corpora.addendum %>% select(-L1)
remove(list=c("verses","artists"))

# Append indexes of lines and phrases -------------------------------------
source("Scripts/DataScrubbingScripts/IndexLinesAndPhrases.R")
corpora.addendum = IndexLinesAndPhrases(corpora.addendum)

# Add to existing corpus --------------
load("DerivedData/FlowBookCorpus.rdata")
corpora = rbind(corpora,corpora.addendum)
#corpora %>% names
corpora = tbl_df(corpora)
corpora = corpora %>% arrange(corpus,artist,verse,beatIndex)
save(list="corpora",file="DerivedData/FlowBookCorpus.rdata")
write.table(corpora,"DerivedData/FlowBookCorpus.txt",quote=F,sep="\t",row.names=F)
#remove(list=c("transc","IndexLinesAndPhrases"))
