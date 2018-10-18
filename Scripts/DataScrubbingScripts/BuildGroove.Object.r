# Load libraries
source("Scripts/loadLibraries.r")

# Load corpus
load("DerivedData/FlowBookCorpus.rdata")

# 1. Define groove.dsegs
source("Scripts/DataScrubbingScripts/groove dsegs (prime form).r")

# 2. Impute verse.dsegs
source("Scripts/DataScrubbingScripts/GetL3Durations.R")

# 3. Build 'grooves' , in which observations are instances of groove dsegs and 
# variables are: effort (the swap distance), start, end, rate (swap distance
# over length), reps, length, adj.length (length penalized by rate), groove, 
# (groove) class, and rotation
source("Scripts/DataScrubbingScripts/GetVerseGrooves.r")

# 4. First time initilization ---------------------------------------------

#grooves = CorrectRotationalEquivalence(GetVerseGrooves("loseYourself1"))
#write.table(file = "DerivedData/corpus_grooves.txt",grooves,quote=F,sep="\t",row.names=F)

# 5. Appending corpora or verse -----------------------------------------------------
# 
corpora.to.detect = c("andre3k")
grooves = fread("DerivedData/corpus_grooves.txt",header=T)
g.verses = unique(grooves$verse)
c.verses = corpora %>% filter(corpus %in% corpora.to.detect) %>%
  .[["verse"]] %>% unique
verseToDo = c.verses[!c.verses %in% g.verses] %>% as.character
didntWork = ""
for(v in verseToDo){
  g1 = CorrectRotationalEquivalence(GetVerseGrooves(v))
  if(identical(colnames(g1),colnames(grooves))){
    write.table(g1,file = "DerivedData/corpus_grooves.txt",
                append=T,quote=F,sep="\t",row.names=F,col.names=F)
  }else{
    didntWork = c(didntWork,v)
  }
}

# 5.5 Revising verses -----------------------------------------------------

grooves = fread("DerivedData/corpus_grooves.txt",header=T)
revise = paste("eminem_renegade2")
revise = laply(revise,function(i) strsplit(i,"_")[[1]][2])
didntWork = ""
for(v in revise){
  g1 = CorrectRotationalEquivalence(GetVerseGrooves(v))
  if(identical(colnames(g1),colnames(grooves))){
    grooves = grooves %>% filter(verse!=v)
    grooves = rbind(grooves, g1)
    write.table(file = "DerivedData/corpus_grooves.txt",
                grooves,quote=F,sep="\t",row.names=F)
  }else{
    didntWork = c(didntWork,v)
  }
  print(v)
}


# # 6. Appending other groove classes ---------------------------------------
# 
# new.groove.dsegs = groove.dsegs[!groove.dsegs %in% existing]
# grooves = fread("DerivedData/corpus_grooves.txt",header=T)
# already.detected = grooves %>% filter(groove %in% new.groove.dsegs) %>% 
#   distinct(verse) %>% .$verse
# 
# g.verses = unique(grooves$verse)
# verseToDo = g.verses[!g.verses %in% already.detected]
# for(v in g.verses){
#   g1 = CorrectRotationalEquivalence(GetVerseGrooves(v,target = new.groove.dsegs))
#   if(identical(colnames(g1),colnames(grooves))){
#     write.table(g1,file = "DerivedData/corpus_grooves.txt",
#                 append=T,quote=F,sep="\t",row.names=F,col.names=F)
#   }else{
#     didntWork = c(didntWork,v)
#   }
#   cat(v)
# }
# grooves = fread("DerivedData/corpus_grooves.txt",header=T)
# grooves = grooves %>% arrange(corpus,verse,start,class,groove)
# write.table(grooves,file = "DerivedData/corpus_grooves.txt",
#             append=F,quote=F,sep="\t",row.names=F,col.names=T)
# 
# 
# Conflate equivalent groove classes --------------------------------------
# 
# grooves = fread("DerivedData/corpus_grooves.txt",header=T)
# grooves$class[which(grooves$class=="332")] = "332332"
# grooves$class[which(grooves$class=="22")] = "22222222"
# grooves$groove[which(grooves$groove=="332")] = "332332"
# grooves$groove[which(grooves$groove=="22")] = "22222222"
# grooves = CorrectRotationalEquivalence(grooves)
# write.table(grooves,file = "DerivedData/corpus_grooves.txt",
#             append=F,quote=F,sep="\t",row.names=F,col.names=T)

