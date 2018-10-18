# A function for tidying the data with respect to beat index, endings, etc.
IndexLinesAndPhrases = function(corpora){
  l = vector()
  for(i in unique(corpora$verse)){
    c2 = corpora %>% filter(verse==i) %>% mutate(index = 1:dim(.)[1])
    l.end = c2 %>% filter(lineEnding==1) %>% .[["index"]]
    l = c(l,laply(c2$index,function(j){
      which(j<=l.end)[1]
    }))
  }
  
  p = vector()
  for(i in unique(corpora$verse)){
    c2 = corpora %>% filter(verse==i) %>% mutate(index = 1:dim(.)[1])
    b.end = c2 %>% filter(breathEnding==1) %>% .[["index"]]
    p = c(p,laply(c2$index,function(j){
      which(j<=b.end)[1]
    }))
  }

  corpora = cbind(corpora,line = l,phrase = p)
  return(corpora)
}
