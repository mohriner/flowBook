# This script takes text files of lyrics found in TextGrids and determines
# (1) any words need to be entered into the CMUPD addendum and
# (2) any words need to added to hyph_dict.txt, the table of hyphenation
# * Note: this script was developed for work on "Pitch Contours in Rap Delivery"
source("Scripts/loadLibraries.r")

tg.path = "/Users/mohriner/Dropbox/Research/Current/PitchContoursInRapDelivery/Data/praatOutput/shutterbugg.TextGrid"

# Get lyrics
tg2matrix = function(TG){
  s2 = grep("item \\[2\\]",TG)
  s3 = grep("item \\[3\\]",TG)
  s4 = grep("item \\[4\\]",TG)
  # Lines with bracketed numbers
  ints = grep("\\[[[:digit:]]",TG)
  l2 = ints[which(ints>s2&ints<s3)]
  l3 = ints[which(ints>s3&ints<s4)]
  sylOnset = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+1]))
  sylOffset = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+2]))
  sylText = gsub("[(text\\=[:space:]\")]","",TG[l2+3])
  wordOnset = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l3+1]))
  wordOffset = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l3+2]))
  wordText = gsub("[(text\\=[:space:]\")]","",TG[l3+3])
  syl = data.frame(sylOnset,sylOffset,sylText)
  word = data.frame(wordOnset,wordOffset,wordText)
  list(syllables = syl,words=word)
}
tg.path %>% readLines %>% tg2matrix -> TG
TG$syllables%>% filter(!sylText %in% c("sp","br")) -> TG$syllables
TG$words%>% filter(!wordText %in% c("sp","br")) -> TG$words
TG$words %>% .$wordText %>% as.character -> lyrics

# Get n.syls
laply(TG$syllables$sylOnset,function(i){
  i %>% add(.025) %>% is_greater_than(TG$words$wordOnset) %>% 
    which %>% last %>% 
    TG$words$wordOnset[.]
}) %>% rle %>% .$lengths -> n.syls
remove(list=c("TG","tg2matrix","tg.path"))

# Load CMUPD to find missing entries -----------------------------------------------
load("/Users/mohriner/Dropbox/Research/RapWork/flowBook/DerivedData/CMU_contours.rdata")

# Remove punctuation and straighten quotes
lyrics %>% toupper %>% 
  gsub("-"," ",.) %>% 
  gsub("[!?;:,.\\(\\)]","",.) %>% 
  gsub("[\u2018\u2019\u201A\u201B\u2032\u2035\u201C\u201d]","",.) %>% 
  gsub("\"","",.) -> lyrics

# Identify words not in the CMU dictionary, add extra lines to 
# "cmudict-syls-addendum.txt" as necessary
in.CMU_contours = laply(1:length(lyrics),function(i){
  CMU_contours %>% 
    filter(word==lyrics[i],n.syl==n.syls[i]) %>% 
    dim %>% .[1] %>% as.logical
})
non.cmu = data.frame(word = lyrics[which(!in.CMU_contours)],n.syl = n.syls[which(!in.CMU_contours)])

# make new entries as necessary.

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
