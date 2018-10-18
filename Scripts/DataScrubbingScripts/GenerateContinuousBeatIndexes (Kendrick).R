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
f = list.files(pattern = "kendrickLamar")

# Read in TextGrids, convert to continuous onsets
bi.cont = llply(f,function(i){
  artist = strsplit(i,"[\\.\\_]")[[1]][1]
  verse = strsplit(i,"[\\.\\_]")[[1]][2]
  TG = readLines(i)
  TextGrid2ContinuousOnset(TG,artist,verse)
},.inform=T,.progress = "text")
setwd("../../../../")
names(bi.cont) = f

#
r = read.delim("SourceData/PhoneticData/flowBeatAlignment/TextGrid_Corpus_alignment.txt",header=T)
# Reconcile lengths (in syllables) of textgrids and the corpora
kendrick.edited.c.bi = llply(1:length(bi.cont),function(i){
  a = strsplit(f[i],"[\\.\\_]")[[1]][1]
  v = strsplit(f[i],"[\\.\\_]")[[1]][2]
  bi.cont1 = bi.cont[i] %>% .[[1]] %>% .$beatIndex.c
  if(a=="kendrickLamar"){ # Kendrick is different because I had continuous data 
    # for him before I had quantized data, so he's not yet in the corpus when
    # I'm reconciling textgrids and CMUPD
    w1 = bi.cont[i] %>% .[[1]] %>% .$word %>% tolower # word in textgrid
    # word in CMUPD
    w2 = v %>% paste("SourceData/VerseTranscriptions/kendrickLamar_",.,".txt",sep="") %>% 
      read.delim2(.,stringsAsFactors=F) %>% .$word
    n1 = length(w1)
    n2 = length(w2)
  }else{
    w1 = bi.cont[i] %>% .[[1]] %>% .$word %>% tolower # word in textgrid
    w2 = corpora %>% filter(artist==a,verse==v) %>% .$word # word in CMUPD
    n1 = bi.cont[i] %>% .[[1]] %>% dim %>% .[1]
    n2 = corpora %>% filter(artist==a,verse==v) %>% dim %>% .[1]
  }
  if(n1>n2){
    w2 = c(w2,rep("",n1-n2))
  }
  if(n2>n1){
    w1 = c(w1,rep("",n2-n1))
  }
  #if(checking==T){data.frame(w1,w2) %>% View}
  
  r1 = r %>% filter(VERSE==v)
  if(dim(r1)[1]!=0){
    w3 = alply(r1,1,function(j){
      if(j$INSERT>=0){w.return = c(w2[j$START:j$STOP],rep(NA,j$INSERT))}
      w.return
    }) %>% unlist %>% unname
    w2 = w3
  }
  
  w1 = w1[nchar(w1)!=0]
  w2 = w2[nchar(w2)!=0]
  #length(bi.cont1)==length(w2)
  #if(checking==T){cat(v)}
  return(bi.cont1)
})
# kendrick.edited.c.bi %>% laply(.,length)
# f %>% gsub("\\.TextGrid","",.) %>% 
#   paste("SourceData/VerseTranscriptions/",.,".txt",sep="") %>% 
#   laply(.,function(i) read.delim2(i) %>% dim %>% .[1])

# Prune the verseTranscriptions files to be the same length as the TextGrids
for(i in 1:length(f)){
  f[i] %>% gsub("\\.TextGrid","",.) %>% 
    paste("SourceData/VerseTranscriptions/",.,".txt",sep="") %>% 
    read.delim2 ->dat
  if("c.bi" %in% names(dat)){dat = dat[,-match("c.bi",names(dat))]}
  if("bi.c" %in% names(dat)){dat = dat[,-match("bi.c",names(dat))]}
  
  f[i] %>% strsplit(.,"_") %>% unlist %>% .[2] %>% 
    gsub("\\.TextGrid","",.) -> title
  r %>% filter(VERSE==title) %>% alply(.,1,function(j){j$START:j$STOP}) %>% 
    unlist %>% unname -> new.seq
  rep1 = r %>% filter(VERSE==title,INSERT==1) %>% .$STOP
  new.seq = sort(c(new.seq,rep1))
  if(length(new.seq)==0){new.seq = 1:dim(dat)[1]}
  dat[new.seq,] %>% tbl_df %>% 
    mutate(bi.c = round(kendrick.edited.c.bi[[i]],digits=3)) -> dat2
  write.table(dat2,paste("SourceData/VerseTranscriptions/kendrickLamar_",title,".txt",sep=""),
              quote=F,row.names=F,sep="\t")
}
