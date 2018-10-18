# This script reads in the syllable-ized CMU pronouncing dictionary, the adden-
# dum prepared for this project, and creates CMU.accents, a list in which each
# item is a word and the list contains vectors of possible accent contours for 
# that word. Multiple entries are necessary for some words because they could
# be transcribed as, say, one or two syllables.


# Load libraries ------------------------------------------------------------

source("Scripts/loadLibraries.r")

# Load CMU objects --------------------------------------------------------

load("DerivedData/CMU_contours.rdata")

# Get new words -----------------------------------------------------------
# This needs to include alternate syllabization of words.

CMU = readLines("SourceData/PhoneticData/cmudict-syls-addendum.txt")
CMU_words = laply(CMU,function(i){
  strsplit(i,"  ")[[1]][1]
},.progress="text")
CMU_words = gsub( " *\\(.*?\\) *", "", CMU_words)
CMU_nsyl = laply(CMU,function(i){
  strsplit(i,"  ")[[1]][2] %>% gregexpr("\\-",.) %>% .[[1]] -> hyphs
  if(any(hyphs==-1)){return(1)}else{return(length(hyphs)+1)}
},.inform=T)
in.CMU_contours = laply(1:length(CMU),function(i){
  CMU_contours %>% 
    filter(word==CMU_words[i],n.syl==CMU_nsyl[i]) %>% 
    dim %>% .[1] %>% as.logical
})

CMU = CMU[!in.CMU_contours]
CMU_words = CMU_words[!in.CMU_contours]

if(length(CMU_words)>0){
  # Load CMU and addendum ---------------------------------------------------
  
  # removes indexing of alternate
  # versions, e.g., "PAD(2)  P AH1 D"
  # Get CMU accent contours -------------------------------------------------
  
  CMU_accentContours = llply(CMU,function(i){
    accentContour = i %>% 
      strsplit(.,"  ") %>% # double space separates word from phonetic entree
      .[[1]] %>% .[[2]] %>% # get second element (first is word)
      gsub("[[:upper:]]","",.) %>% # remove letters and blanks
      gsub("[[:blank:]]","",.) %>% 
      strsplit(.,"-") %>% unlist %>% as.numeric # split by hyphen, convert to int
    if(length(accentContour)==1){accentContour = NA} # monosyllables always NA
    accentContour
  },.inform=T)
  
  # Remove secondary accents not surrounded by non-accents-------------------
  RemoveSecondaryAccents = function(contour){
    keep = c("002","102","200","201","202","020")
    if(length(contour)>2){
      for(i in 1:(length(contour)-2)){
        c2 = contour[i:(i+2)]
        if(!paste(c2,collapse="") %in% keep){
          if(2 %in% c2){
            c2[which(c2==2)] = 0
          }
        }else{
          c2[which(c2==2)] = 1
        }
        contour[i:(i+2)] = c2
      }
    }else{
      if(2 %in% contour){
        contour[which(contour==2)] = 0
      }
    }
    return(contour)
  }
  
  CMU_accentContoursNoSecondary = llply(CMU_accentContours,
                                        RemoveSecondaryAccents,.inform=T)
  
  # Build data frame with word, number of syllables, and contour-------------
  
  x = laply(CMU_accentContoursNoSecondary,function(i) paste(i,collapse=""))
  CMU_contours.1 = data.frame(word = CMU_words,n.syl = nchar(x),contour = x)
  CMU_contours.1$n.syl[which(CMU_contours.1$contour=="NA")] = 1
  CMU_contours.1$contour[which(CMU_contours.1$n.syl==0)] = NA
  CMU_contours.1$contour[which(CMU_contours.1$contour=="NA")] = NA
  CMU_contours.1$word = as.character(CMU_contours.1$word)
  CMU_contours.1$contour = as.character(CMU_contours.1$contour)
  CMU_contours.1 %>% group_by(word,n.syl,contour) %>% slice(1) %>% ungroup -> CMU_contours.1
  CMU_contours = rbind(CMU_contours,CMU_contours.1)
  save(list="CMU_contours",file = "DerivedData/CMU_contours.rdata")
  
  remove(list=c("CMU_contours.1","CMU","CMU_accentContours","CMU_accentContoursNoSecondary",
                "x","RemoveSecondaryAccents"))
}else{
  remove(list=c("CMU","CMU_words"))
}