# This script reads in the syllable-ized CMU pronouncing dictionary, the adden-
# dum prepared for this project, and creates CMU.accents, a list in which each
# item is a word and the list contains vectors of possible accent contours for 
# that word. Multiple entries are necessary for some words because they could
# be transcribed as, say, one or two syllables.


# Load libraries ------------------------------------------------------------

source("Scripts/loadLibraries.r")

# Load CMU and addendum ---------------------------------------------------

CMU = c(readLines("SourceData/PhoneticData/cmudict.txt"),
        readLines("SourceData/PhoneticData/cmudict-syls-addendum.txt"))
CMU = CMU[-which(substr(CMU,1,1)=="#")]
CMU_words = laply(CMU,function(i){
  strsplit(i,"  ")[[1]][1]
},.progress="text")
CMU_words = gsub( " *\\(.*?\\) *", "", CMU_words) # removes indexing of alternate
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
},.progress="text",.inform=T)

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
                           RemoveSecondaryAccents,
                           .progress="text",.inform=T)

# Build data frame with word, number of syllables, and contour-------------

x = laply(CMU_accentContoursNoSecondary,function(i) paste(i,collapse=""))
CMU_contours = data.frame(word = CMU_words,n.syl = nchar(x),contour = x)
CMU_contours$n.syl[which(CMU_contours$contour=="NA")] = 1
CMU_contours$contour[which(CMU_contours$n.syl==0)] = NA
CMU_contours$contour[which(CMU_contours$contour=="NA")] = NA
CMU_contours$word = as.character(CMU_contours$word)
CMU_contours$contour = as.character(CMU_contours$contour)
CMU_contours %>% group_by(word,n.syl,contour) %>% slice(1) %>% ungroup -> CMU_contours
save(list="CMU_contours",file = "DerivedData/CMU_contours.rdata")


# The same, but with secondary accents ------------------------------------

# (comment out lines 35-62 before continuing)

#x = laply(CMU_accentContours,function(i) paste(i,collapse=""))
#CMU_contours = data.frame(word = CMU_words,n.syl = nchar(x),contour = x)
#CMU_contours$n.syl[which(CMU_contours$contour=="NA")] = 1
#CMU_contours$contour[which(CMU_contours$n.syl==0)] = NA
#CMU_contours$contour[which(CMU_contours$contour=="NA")] = NA
#save(list="CMU_contours",file = "DerivedData/CMU_contours_withSecondary.rdata")

#--- To correct an isolated error without rebuilding the whole object
#CMU_contours[which(CMU_contours$word=="GIRLS"),] = 3
#CMU_contours[which(CMU_contours$word=="AMISTAD"),3] = "101"
