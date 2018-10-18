# This script takes lyrics and creates a data.frame to be filled in by the 
# transcriber. The data.frame has 8 columns--word, syllable, quant (defaulted to 
# 16), duration (defaulted to 1), rhymeClass (defaulted to NA), rhymeIndex,
# lineEnding, and breathEnding. It writes this data.frame into the 
# VerseTranscriptions folder, where they can be edited to contain transcriptions
# of rhythm.

source("Scripts/loadLibraries.r")

# 0. Get the piece titles
tg.path = "/Users/mohriner/Dropbox/Research/Current/PitchContoursInRapDelivery/Data/praatOutput/shutterbugg.TextGrid"
verse = "bigBoi_shutterbugg"
# Load data (phonetic, etc.) ----------------------------------------------

# Run the loop that builds the matrices--------

# Get lyrics, syllables, lines, and breathEndings
tg2matrix = function(TG){
  s1 = grep("item \\[1\\]",TG)
  s2 = grep("item \\[2\\]",TG)
  s3 = grep("item \\[3\\]",TG)
  s4 = grep("item \\[4\\]",TG)
  s5 = grep("item \\[5\\]",TG)
  s6 = grep("item \\[6\\]",TG)
  
  # Lines with bracketed numbers
  ints = grep("\\[[[:digit:]]",TG)
  l1 = ints[which(ints>s1&ints<s2)]
  l2 = ints[which(ints>s2&ints<s3)]
  l3 = ints[which(ints>s3&ints<s4)]
  l4 = ints[which(ints>s4&ints<s5)]
  l5 = ints[which(ints>s5&ints<s6)]
  l6 = ints[which(ints>s6)]
  onset = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+1]))
  offset = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+2]))
  phone = gsub("[(text\\=[:space:]\")]","",TG[l1+3])
  on2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+1]))
  off2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+2]))
  lab2 = gsub("[(text\\=[:space:]\")]","",TG[l2+3])
  on3 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l3+1]))
  off3 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l3+2]))
  lab3 = gsub("[(text\\=[:space:]\")]","",TG[l3+3])
  on4 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l4+1]))
  off4 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l4+2]))
  lab4 = gsub("[(text\\=[:space:]\")]","",TG[l4+3])
  on5 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l5+1]))
  off5 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l5+2]))
  lab5 = gsub("[(text\\=[:space:]\")]","",TG[l5+3])
  on6 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l6+1]))
  off6 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l6+2]))
  lab6 = gsub("[(text\\=[:space:]\")]","",TG[l6+3])
  
  ph = data.frame(onset,offset,phone)
  syl = data.frame(on2,off2,lab2)
  word = data.frame(on3,off3,lab3)
  interPhrase = data.frame(on4,off4,lab4)
  intonPhrase = data.frame(on5,off5,lab5)
  p.e. = data.frame(on6,off6,lab6)
  list(phones=ph,syllables = syl,words=word,interPhrase = interPhrase,
       intonPhrase = intonPhrase,
       pitch.errors = p.e.)
}

tg.path %>% readLines %>% tg2matrix -> TG
TG$syllables%>% filter(!lab2 %in% c("sp","br")) -> TG$syllables
TG$syllables[,3] = as.character(TG$syllable[,3])
TG$words%>% filter(!lab3 %in% c("sp","br")) -> TG$words
TG$words %>% .$lab3 %>% as.character -> lyrics

# Get n.syls
w.onsets = laply(TG$syllables[,1],function(i){
  i %>% add(.025) %>% is_greater_than(TG$words[,1]) %>% 
    which %>% last %>% 
    TG$words[,1][.]
})
rle(w.onsets)$lengths %>% 
  llply(.,function(i) 1:i) %>%
  llply(.,function(i){
    if(length(i)==1){return(FALSE)}else{
      return(c(rep(TRUE,length(i)-1),FALSE))
    }
  }) %>% unlist -> need.hyph
TG$syllables[,3][need.hyph] = paste(as.character(TG$syllables[,3])[need.hyph],"-",sep="")

# Get lineEnding
laply(TG$syllables[,1],function(i){
  i %>% add(.025) %>% is_greater_than(TG$interPhrase[,1]) %>% 
    which %>% last
}) %>% rle %>% .$lengths %>% cumsum -> ph.end
lineEnding = rep(0,dim(TG$syllables)[1])
lineEnding[ph.end] = 1

# Get breathEnding
tg.path %>% readLines %>% tg2matrix %>% 
  .$syllable %>% 
  filter(lab2=="br") %>% 
  .$on2 -> br
laply(TG$syllables[,1],function(i){
  i %>% add(.025) %>% is_greater_than(c(0,br)) %>% 
    which %>% last
}) %>% rle %>% .$lengths %>% cumsum  -> br.end
breathEnding = rep(0,dim(TG$syllables)[1])
breathEnding[br.end] = 1
# 
dat = data.frame(word = TG$words[,3] %>% as.character %>% rep(.,rle(w.onsets)$lengths) %>% tolower,
          syllable = TG$syllables[,3] %>% tolower,
          quant = rep(16,dim(TG$syllables)[1]),
          duration = rep(1,dim(TG$syllables)[1]),
          rhymeClass = rep(NA,dim(TG$syllables)[1]),
          rhymeIndex = rep(NA,dim(TG$syllables)[1]),
          lineEnding = lineEnding,
          breathEnding = breathEnding)

write.table(dat,paste("SourceData/VerseTranscriptions/",verse,".txt",sep=""),sep="\t",row.names=F)
remove(list=c("dat","br","br.end","breathEnding","lineEnding","lyrics","need.hyph",
              "ph.end","TG","tg.path","verse","w.onsets","tg2matrix"))
# Move on to "3 Proofreading Rhythm, Accent, and Rhyme.r"