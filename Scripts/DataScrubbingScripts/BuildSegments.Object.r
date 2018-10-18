# Initialization ----------------------------------------------------------

# Load libraries
source("Scripts/loadLibraries.r")


# Load grooves
grooves = fread("DerivedData/corpus_grooves.txt",header=T)

g.verses = unique(grooves$verse)
# Source the segmentation function
source("Scripts/DataScrubbingScripts/SegmentVerseGrooves.r")

# Load corpus
load("DerivedData/FlowBookCorpus.rdata")

# Load segments
segments = fread("DerivedData/corpus_groove_segments.txt",header=T)

# # First time initialization: ----
# segments = SegmentVerseGrooves(g.verses[1])
# write.table(file = "DerivedData/corpus_groove_segments.txt",segments,quote=F,sep="\t",row.names=F)

# s.verses = unique(segments$verse)
# g.verses = unique(grooves$verse)
# verseToDo = g.verses[!g.verses %in% s.verses]
# for(v in verseToDo){
#   s1 = SegmentVerseGrooves(v)
#   if(identical(colnames(s1),colnames(segments))){
#     write.table(s1,file = "DerivedData/corpus_groove_segments.txt",
#                 append=T,quote=F,sep="\t",row.names=F,col.names=F)
#     cat(v)
#   }else{
#     didntWork = c(didntWork,v)
#   }
# }

# Revise ------------------------------------------------------------------

revise = paste("eminem_renegade2")
revise = laply(revise,function(i) strsplit(i,"_")[[1]][2])
for(v in revise){
  s1 = SegmentVerseGrooves(v)
  if(identical(colnames(s1),colnames(segments))){
    segments = segments %>% filter(verse!=v)
    segments = rbind(segments, s1)
    write.table(file = "DerivedData/corpus_groove_segments.txt",
                segments,quote=F,sep="\t",row.names=F)
  }else{
    didntWork = c(didntWork,v)
  }
  print(v)
}