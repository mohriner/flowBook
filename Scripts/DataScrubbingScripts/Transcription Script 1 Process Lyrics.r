# This script takes text files of lyrics and determines if 
# (1) any words need to be entered into the CMUPD addendum and
# (2) any words need to added to hyph_dict.txt, the table of hyphenation
source("Scripts/loadLibraries.r")

# Load CMUPD to find missing entries -----------------------------------------------
load("DerivedData/CMU_contours.rdata")

# Identify lyrics to process

# Identify non-CMU words, no special functions AND NO LOOPS
# Load lyrics
setwd("~/Desktop/andre new lyrics/")
lyrics = lapply(list.files(),function(x) {
  scan(paste(x,sep=""),quote=NULL,what="")
  })
setwd("../../..")
lyrics = scan("SourceData/Lyrics/transcriptions (line)/arrestedDevelopment_living2.txt",quote=NULL,what="")

# Remove punctuation and straighten quotes
lyrics = lapply(lyrics,function(x) toupper(gsub("-"," ",x)) %>% strsplit(.," ") %>% unlist)
lyrics = lapply(lyrics,function(x) toupper(gsub("[!?;:,.\\(\\)]","",x)))
lyrics = lapply(lyrics,function(x) toupper(gsub("[\u2018\u2019\u201A\u201B\u2032\u2035\u201C\u201d]","",x)))
lyrics = lapply(lyrics,function(x) toupper(gsub("\"","",x)))

# Identify words not in the CMU dictionary, add extra lines to 
# "cmudict-syls-addendum.txt" as necessary
non.cmu = llply(lyrics,function(l){
  l[which(!(l %in% CMU_contours$word))]
}) %>% unlist %>% sort
# look at non.cmu, add to the addendum
write(non.cmu,"~/Desktop/non.cmu.txt",ncolumns=1)
remove(list="non.cmu")

# Append new CMU entries --------------------------------------------------

source("Scripts/DataScrubbingScripts/AppendCMU.Accent.r")
#list.files()[laply(lyrics,function(i) "SHES" %in% i)]

# Check hyphenation -------------------------------------------------------

# Get hyphenation
hyph_dict = readLines("SourceData/PhoneticData/hyph_dict.txt")
hyph_dict = as.vector(unlist(sapply(hyph_dict,function(x) return(strsplit(x," ")[[1]][2]))))

# Remove monosyllables
lyrics = lyrics %>% unlist 
lyrics = lyrics[which(CMU_contours$n.syl[match(lyrics,CMU_contours$word)] !=1)]
need.hyphen = lyrics %>% tolower %>% is_in(hyph_dict) %>% not %>% 
  which %>% lyrics[.] %>% unique

# Find needed entries to hyph.dict
cmu.match = match(need.hyphen, CMU_contours$word)
n.syls = CMU_contours$n.syl[cmu.match]
need.hyphen = need.hyphen[order(n.syls)] %>% tolower
write.table(matrix(rep(need.hyphen,2),length(need.hyphen),2),"~/Desktop/need-hyphen.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
remove(list="CMU_contours","cmu.match","hyph_dict","lyrics","n.syls","need.hyphen","titles")

# Move on to "Build Matrix Template.r"
