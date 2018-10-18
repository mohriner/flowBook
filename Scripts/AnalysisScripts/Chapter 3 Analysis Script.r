# Endnote 6: Calculate distribution of CMUPD word accent types by mod-4 posit --------

# Read in genre1 source data -
meta = read.delim("SourceData/verseMetadata.txt") %>% filter(tranche=="genre1") %>% tbl_df

setwd("SourceData/VerseTranscriptions/")
f = paste(meta$artist,"_",meta$title,".txt",sep="")
titles = unlist(strsplit(f,".txt"))
source.transcriptions = llply(f,function(i) read.delim(i,header=T))
setwd("../..")
remove(list="f")

# Append CMU_accent -

source("Scripts/DataScrubbingScripts/AppendCMU.Accent.r")
transc = llply(source.transcriptions,AppendCMU.Accent,.inform=T,.progress="text")
names(transc) = titles

# Append Beat Index -
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
corpora = melt(unname(transc),id.vars = 1:dim(transc[[1]])[2])
corpora = tbl_df(corpora)

# Get percentage that are in C16
corpora %>% mutate(mod4 = (beatIndex%%1)*4) %>%  
  group_by(inC16 = mod4 %% 1 == 0) %>% summarize(n = length(word))

# Get distribution
distrib = corpora %>% mutate(mod4 = (beatIndex%%1)*4) %>% filter(mod4 %% 1 ==0) %>% 
  group_by(CMU_accent,mod4) %>% summarize(n = length(word)) %>% 
  acast(.,CMU_accent~mod4)

# Print out:
#       0    1    2    3
# 0   227 1025  335  595
# 1  1164  148  571  275
# 2    47   34   71   25
# NA 2232 1534 2547 2219


# Endnote 10: Percentage of syllables in genre-wide corpus manually corrected ----
load("DerivedData/FlowBookCorpus.rdata")

corpora %>% 
  filter(corpus=="genre1") %>% 
  group_by(toggled) %>% 
  summarize(n=length(word)) %>% 
  mutate(prop = round(n/sum(n),digits=3)) # .046

# Endnote 11: How likely are function monosyllables vs. non-function monosyllables to have their accent stripped? ------------
load("DerivedData/FlowBookCorpus.rdata")
funcWords = readLines("SourceData/Other/functionWords.txt") %>% strsplit(.," ") %>% unlist
funcCount = corpora %>% mutate(a1=accent) %>% filter(corpus=="genre1")
class(funcCount$accent) = "logical"
class(funcCount$a1) = "logical"
funcCount$a1[which(funcCount$toggled==TRUE)] = !funcCount$accent[which(funcCount$toggled==1)]
unAcNonFunc = funcCount %>% mutate(func = toupper(word) %in% funcWords) %>% 
  filter(func==FALSE,accent==TRUE) %>% 
  dim %>% .[1]
unAcFunc = funcCount %>% mutate(func = toupper(word) %in% funcWords) %>% 
  filter(func==TRUE,accent==TRUE) %>% 
  dim %>% .[1]
toggledUnAcNonFunc = corpora %>% filter(corpus=="genre1",is.na(CMU_accent)) %>%
  mutate(func = toupper(word) %in% funcWords) %>% 
  filter(accent==0,func==0,toggled==TRUE) %>% 
  dim %>% .[1]
toggledUnAcFunc = corpora %>% filter(corpus=="genre1",is.na(CMU_accent)) %>%
  mutate(func = toupper(word) %in% funcWords) %>% 
  filter(accent==0,func==1,toggled==TRUE) %>% 
  dim %>% .[1]
print((toggledUnAcFunc/unAcFunc)/(toggledUnAcNonFunc/unAcNonFunc)) # 12.5 times as likely
remove(list=c("funcCount","funcWords","unAcNonFunc","unAcFunc","toggledUnAcNonFunc","toggledUnAcFunc"))

