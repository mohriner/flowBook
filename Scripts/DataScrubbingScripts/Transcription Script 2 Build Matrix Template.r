# This script takes lyrics and creates a data.frame to be filled in by the 
# transcriber. The data.frame has 8 columns--word, syllable, quant (defaulted to 
# 16), duration (defaulted to 1), rhymeClass (defaulted to NA), rhymeIndex,
# lineEnding, and breathEnding. It writes this data.frame into the 
# VerseTranscriptions folder, where they can be edited to contain transcriptions
# of rhythm.

source("Scripts/loadLibraries.r")

# Load data (phonetic, etc.) ----------------------------------------------

hyph_dict = readLines("SourceData/PhoneticData/hyph_dict.txt")
hyph_dict = laply(hyph_dict,function(x) return(strsplit(x," ")))
hyph_df = data.frame(word = laply(hyph_dict,function(x) x[2]),
                     hyph = laply(hyph_dict,function(x) x[1])) %>% tbl_df

onset.data = read.delim("SourceData/verseMetadata.txt",header=T)
load("DerivedData/CMU_contours.rdata")

# 0. Get the piece titles
list.files("SourceData/Lyrics/transcriptions (breath)/",pattern="arrested") %>% 
  gsub("\\.txt","",.) -> verses

# Run the loop that builds the matrices--------
# The hazard here is that sometimes the number of syllables returned from 
# the hyphenation dictationary and the CMUPD don't match, so NAs have to be
# added to some of the vectors so that the data frame has a consistent length.
# Then, in transcribing rhythm, one has to find where the extra or missing
# syllables are, insert or remove as necessary, and erase the terminal NAs.
#completed = rep(0,length(verses))
s = which(completed==0)[1]
for(h in s:length(verses)){
  piece = strsplit(verses[h],"_")[[1]][2]
  index1 =which(onset.data$title==piece)
  artist = as.vector(unlist(onset.data$artist[index1]))
  
  # Get count of syllables (from CMUPD and hyphenation)
  
  # 1. Compare hyphenation in hyph_dict to hyphenation in CMU_syls
  path = paste("SourceData/Lyrics/transcriptions (line)/",artist,"_",piece,".txt",sep="")
  lyrics = readLines(path)
  lyrics = lapply(lyrics,function(x) toupper(gsub("-"," ",x)) %>% strsplit(.," ") %>% unlist)
  lyrics = lapply(lyrics,function(x) toupper(gsub("[!?;:,.\\(\\)]","",x)))
  lyrics = lapply(lyrics,function(x) toupper(gsub("[\u2018\u2019\u201A\u201B\u2032\u2035\u201C\u201d]","",x)))
  lyrics = lapply(lyrics,function(x) toupper(gsub("\"","",x)))
  lyrics = llply(lyrics,tolower)
  lyrics = llply(lyrics,function(i){
    if(any(i=="")){i = i[-which(i=="")]}
    i
  })
  if(any(laply(lyrics,function(i) identical(i,"")))){
    lyrics = lyrics[-which(laply(lyrics,function(i) identical(i,"")))]
  }
  
  # Count syllables in hyphenation
  hyph.syls = llply(lyrics,function(line){
    syls = laply(line, function(word){
      i = match(word,hyph_df$word)
      if(is.na(i)){return(1)}else{
        strsplit(hyph_df$hyph[i] %>% as.character,"-") %>% 
          unlist %>% length %>% return
      }
    })
    sum(syls)
  }) %>% unlist %>% sum
  
  # Count syllables in CMU
  cmu.syls = llply(lyrics,function(line){
    syls = laply(line, function(word){
      i = match(toupper(word),CMU_contours$word)
      CMU_contours$n.syl[i]
    })
    sum(syls)
  }) %>% unlist %>% sum
  # 
  n.syls = max(c(cmu.syls,hyph.syls))
  
  #### Feature Specification (including many dummy values) ####
  # 1. word
  reps = lyrics %>% unlist %>% toupper %>% match(.,CMU_contours$word) %>% CMU_contours$n.syl[.]
  word = lyrics %>% unlist %>% rep(.,reps)
  
  # 2. syllable
  syllable = llply(lyrics %>% unlist,function(w){
    if(is.na(match(w,hyph_df$word))){
      return(w)
    }else{
      w1 = hyph_df$hyph[match(w,hyph_df$word)] %>% as.character %>% strsplit(.,"\\-") %>% unlist
      w1 = laply(w1[1:(length(w1)-1)],function(w){
        paste(w,"-",sep="")
      }) %>% c(.,w1[length(w1)])
      return(unname(w1))
      
    }
  }) %>% unlist
  
  # 7. lineEnding
  lineEnding = llply(lyrics,function(line){
    syls = laply(line, function(word){
      i = match(toupper(word),CMU_contours$word)
      CMU_contours$n.syl[i]
    })
    c(rep(0,sum(syls)-1),1)
  },.inform=T) %>% unlist
  
  # 8. breathEnding 
  path = paste("SourceData/Lyrics/transcriptions (breath)/",artist,"_",piece,".txt",sep="")
  lyrics = readLines(path)
  lyrics = lapply(lyrics,function(x) toupper(gsub("-"," ",x)) %>% strsplit(.," ") %>% unlist)
  lyrics = lapply(lyrics,function(x) toupper(gsub("[!?;:,.\\(\\)]","",x)))
  lyrics = lapply(lyrics,function(x) toupper(gsub("[\u2018\u2019\u201A\u201B\u2032\u2035\u201C\u201d]","",x)))
  lyrics = lapply(lyrics,function(x) toupper(gsub("\"","",x)))
  lyrics = lapply(lyrics,function(x) toupper(gsub("\t","",x)))
  lyrics = llply(lyrics,tolower)
  lyrics = llply(lyrics,function(i){
    if(any(i=="")){i = i[-which(i=="")]}
    i
  })
  if(any(laply(lyrics,function(i) identical(i,"")))){
    lyrics = lyrics[-which(laply(lyrics,function(i) identical(i,"")))]
  }
  
  breathEnding = llply(lyrics,function(line){
    syls = laply(line, function(word){
      i = match(toupper(word),CMU_contours$word)
      CMU_contours$n.syl[i]
    })
    c(rep(0,sum(syls)-1),1)
  },.inform=T) %>% unlist
  n.syls = max(c(
    length(word),
    length(syllable),
    length(lineEnding),
    length(breathEnding)
  ))
  
  # Make all the lengths equal
  if(length(word)!=n.syls){        word = c(word,rep(NA,n.syls-length(word)))}
  if(length(syllable)!=n.syls){    syllable = c(syllable,rep(NA,n.syls-length(syllable)))}
  if(length(lineEnding)!=n.syls){  lineEnding = c(lineEnding,rep(NA,n.syls-length(lineEnding)))}
  if(length(breathEnding)!=n.syls){breathEnding = c(breathEnding,rep(NA,n.syls-length(breathEnding)))}
  
  # Add in dummy values
  quant = rep(16,n.syls)
  duration = rep(1,n.syls)
  rhymeClass = rep(NA,n.syls)
  rhymeIndex = rep(NA,n.syls)
  # Build the data.frame
  dat = data.frame(word,syllable,quant,duration,rhymeClass,rhymeIndex,lineEnding,breathEnding)
  
  write.table(dat,paste("SourceData/VerseTranscriptions/",verses[h],".txt",sep=""),sep="\t",row.names=F)
  completed[h] = 1
}
# Move on to "3 Proofreading Rhythm, Accent, and Rhyme.r"