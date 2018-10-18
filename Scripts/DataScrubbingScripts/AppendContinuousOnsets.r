source("Scripts/loadLibraries.r")
load(file = "DerivedData/FlowBookCorpus.rdata")
load("DerivedData/continuous.beat.indexes.rdata")

#
continuousVerses = c.bi %>% names
#
continuousVersesAlreadyInCorpora = corpora %>% 
  filter(corpus!="kendrickLamar") %>% 
  group_by(verse) %>% 
  summarize(cont = !all(is.na(bi.c))) %>% 
  filter(cont==T) %>% 
  .$verse %>% as.character
toDo = continuousVerses[!(continuousVerses %in% continuousVersesAlreadyInCorpora)]
for(v in toDo){
  n1 = length(corpora$bi.c[which(corpora$verse==v)])
  n2 = length(c.bi[[v]])
  if(n1==n2){
    corpora$bi.c[which(corpora$verse==v)] = c.bi[[v]]
  }
}
#corpora %>% filter(verse=="aroundMyWay2") %>% View
corpora = tbl_df(corpora)
corpora = corpora %>% arrange(corpus,artist,verse,beatIndex)
save(list="corpora",file="DerivedData/FlowBookCorpus.rdata")
write.table(corpora,"DerivedData/FlowBookCorpus.txt",quote=F,sep="\t",row.names=F)
