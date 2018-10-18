# This is the first script in getting continuous onsets. TextGrid2ContinuousOnset.r
# provides the function that does a lot of the work.TextGrid_Corpus_alignment.txt
# provides the instructions for adding or deleting syllables from the TextGrids
# so they have the same dimensions as the corpus.

# This script takes the TextGrids that store the onsets (in seconds) of phonemes
# and words in 30 verses by Talib Kweli (the subject of Chapter 8) and 13 verses
# of the larger genre-wide corpus (used for comparison). The work involves (1)
# converting the TextGrid output of Praat into matrices and (2) reconciling the
# different lengths of the matrices from those implied by the corpus. These
# differences might be because there's some non-main-emcee words in the acappella
# file, or because I annotated a word as two syllables in one place and one in 
# another (e.g., fire, every as 2 vs. 3 syllables, etc.)

source("Scripts/loadLibraries.r")
source("Scripts/DataScrubbingScripts/TextGrid2ContinuousOnset.r")
load(file = "DerivedData/FlowBookCorpus.rdata")

vowels = c("AA","AE","AH","AO","AW","AY","EH","ER","EY","IH","IY","OW","OY","UH","UW")
vowels = sort(c(paste(vowels,"0",sep=""),paste(vowels,"1",sep=""),paste(vowels,"2",sep="")))
dict_hyph = read.delim("SourceData/PhoneticData/hyph_dict.txt",sep=" ",stringsAsFactors=F)
time_dat = read.delim("SourceData/verseAcapellaMetadata.txt",header=T)

# Get verses with acapellas
setwd("SourceData/PhoneticData/flowBeatAlignment/CorrectedTextGrids/")
f = list.files()

# Read in TextGrids, convert to continuous onsets
bi.cont = llply(f,function(i){
  artist = strsplit(i,"[\\.\\_]")[[1]][1]
  verse = strsplit(i,"[\\.\\_]")[[1]][2]
  TG = readLines(i)
  TextGrid2ContinuousOnset(TG,artist,verse)
},.inform=T,.progress = "text")
setwd("../../../../")
names(bi.cont) = f
r = read.delim("SourceData/PhoneticData/flowBeatAlignment/TextGrid_Corpus_alignment.txt",header=T)
# Reconcile lengths (in syllables) of textgrids and the corpora
revised.bi.cont = llply(1:length(bi.cont),function(i){
  a = strsplit(f[i],"[\\.\\_]")[[1]][1]
  v = strsplit(f[i],"[\\.\\_]")[[1]][2]
  bi.cont1 = bi.cont[i] %>% .[[1]] %>% .$beatIndex.c
  w1 = bi.cont[i] %>% .[[1]] %>% .$word %>% tolower # word in textgrid
  w2 = corpora %>% filter(artist==a,verse==v) %>% .$word # word in CMUPD
  n1 = bi.cont[i] %>% .[[1]] %>% dim %>% .[1]
  n2 = corpora %>% filter(artist==a,verse==v) %>% dim %>% .[1]
  if(n1>n2){
    w2 = c(w2,rep("",n1-n2))
  }
  if(n2>n1){
    w1 = c(w1,rep("",n2-n1))
  }
  #data.frame(w2,w1) %>% View
  r1 = r %>% filter(VERSE==v)
  if(dim(r1)[1]!=0){
    w3 = alply(r1,1,function(j){
      if(j$INSERT>=0){w.return = c(w1[j$START:j$STOP],rep(NA,j$INSERT))}
      w.return
    }) %>% unlist %>% unname
    w1 = w3
    bi3 = alply(r1,1,function(j){
      if(j$INSERT>=0){bi.return = c(bi.cont1[j$START:j$STOP],rep(NA,j$INSERT))}
      bi.return
    }) %>% unlist %>% unname
    bi.cont1 = bi3
  }
  #if(any(w2=="")){w2 = w2[-which(w2=="")]}
  length(bi.cont1)==n2
  bi.cont1
})
names(revised.bi.cont) = laply(f,function(i) strsplit(i,"[\\.\\_]")[[1]][2])
c.bi = revised.bi.cont
save(list=c("c.bi"),file="DerivedData/continuous.beat.indexes.rdata")
remove(list=c("v"))
