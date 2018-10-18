# This function takes a source transcription and appends a column reflecting
# CMU accent of each syllable.
# Load or source CMU_contours data frame ----------------------------------

#load("DerivedData/CMU_contours_withSecondary.rdata") # use this line for the calculation
# of Example 3.5.
load("DerivedData/CMU_contours.rdata") # ordinarily use this line.
# source("Scripts/DataScrubbingScripts/BuildCMU.Objects.r)
AppendCMU.Accent = function(dat){
  # Get the sequence of words and their asserted number of syllables. This would
  # be a simple matter of run-length encoding, but some of the words are repeated.
  ending.syl = which(!grepl("\\-",dat$syllable %>% as.character))
  n.syls = diff(c(0,ending.syl))
  words = dat$word[ending.syl] %>% as.character %>% toupper
  
  #
  CMU_accent = llply(1:length(words),function(i){
    CMU_contours %>% 
      filter(word==words[i],n.syl == n.syls[i]) %>% 
      slice(1) %>% 
      .[["contour"]] %>%
      as.character %>% 
      strsplit(.,"") %>% 
      unlist %>% 
      as.numeric
  }, .inform=T)
  names(CMU_accent) = words
  CMU_accent = CMU_accent%>% unlist
  # rle(dat$word %>% as.character)$lengths
  # laply(CMU_accent,length)
  # rle(dat$word %>% as.character)$lengths == laply(CMU_accent,length)
  # words[which(laply(CMU_accent,length)==0)]
  # words[108]
  if(dim(dat)[1]==length(CMU_accent)){
    dat = cbind(dat,CMU_accent)
    return(dat)
  }else{
    return("CMU_accent different length than data.frame")
  }
}