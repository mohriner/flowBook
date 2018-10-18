# Section 1: The functions -----------------------------------------------------

getIRIs = function(c1,method = c("consecutive","all"),return.list=F){
  upperLimit = 8 # Rhymes more than 8 beats apart are not understood as rhymes.
  # There may be a better empirical measure for this; "8" is my own speculation.
  # The timepoint of a rhyme is defined as the last syllable in the rhyme
  # class to receive an accent in at least one instance
  lastAccent = c1 %>% 
    filter(!is.na(rhymeClass)) %>% 
    group_by(rhymeClass,rhymeIndex,verse) %>% 
    summarize(accented = any(accent==1)) %>% 
    arrange(verse,rhymeClass,rhymeIndex) %>% 
    filter(accented==TRUE) %>% 
    group_by(verse,rhymeClass) %>% 
    slice(n())
  if(dim(lastAccent)[1]==0){return(NA)}
  rhymePoints = llply(1:dim(lastAccent)[1],function(i){
    c1 %>% filter(rhymeClass==lastAccent$rhymeClass[i],
                  rhymeIndex==lastAccent$rhymeIndex[i]) %>% 
      .$beatIndex
  })
  
  # Measure durations between either consecutive instances or all pairs. The
  # latter is a better method, the former used only for argumentation in the 
  # text.
  if(method == "consecutive"){
    IRIs = llply(rhymePoints,diff)
  }else{
    IRIs = llply(rhymePoints,function(i){
      if(length(i)==1){
        return(NA)
      }else{
        return(combn(i,2) %>% apply(.,2,diff))
      }
    })
  }
  IRIs = llply(IRIs,function(i) if(length(i)==0){return(NA)}else{return(i)})
  if(return.list==TRUE){
    return(IRIs)
  }
  IRIs = IRIs %>% unlist
  IRIs = IRIs[IRIs<=upperLimit]
  IRIs %>% round(digits=2)
}

# A function for retrieving the IRIs of a verse
Verse2IRIs = function(title,method = c("consecutive","all"),return.list=F){
  c1 = corpora %>% filter(verse==title)
  IRIs = getIRIs(c1,method,return.list)
  if(return.list==T){
    return(IRIs)
  }else{
    return(IRIs %>% round(digits=2))
  }
}

# A function for calculating the IRI entropy of a verse (or segment of a verse),
# possibly with a "mod4" condition, converts IRIs to mod-1 IRIs before calculating
# entropy.
getVerseEntropy = function(title,row.index = NA, mod4 = FALSE){
  upperLimit = 8
  # Isolate the verse from the corpus data frame.
  c1 = corpora %>% filter(verse==title)
  if(!is.na(row.index[1])){
    c1 = c1 %>% slice(row.index)
  }
  # Get the IRIs, mod4 if requested
  IRIs = getIRIs(c1,method = "all")
  if(mod4==TRUE){IRIs = IRIs %% 1}
  # Tabulate IRIs, including a tabulation of '0' for all extent IRIs that do 
  # not occur in the verse. (This is to ensure that all measures of entropy
  # have the same number of bins).
  if(mod4==TRUE){
    t = table(c(IRIs,vals.mod4))-1
  }else{
    t = table(c(IRIs,vals))-1
  }
  # Get entropy, normalize by the log of the number of bins. (This makes 
  # entropy a value between 0 and 1.)
  ent = entropy(t)
  ent = ent/log(length(t))
  ent
}

# Section 2: Some global variables ----------------------------------------

# Determine all the values for IRIs in the system, so that measures of entropy
# all have the same number of bins.
#titles.val = corpora %>% filter(corpus=="genre1") %>% .$verse %>% unique
#vals = llply(titles.val, Verse2IRIs, method = "all",.progress="text",.inform=T) %>% unlist %>% unique %>% sort
vals = scan("SourceData/Other/IRI_values.txt")
#vals.mod4 = llply(titles.val, Verse2IRIs,method = "all",.progress="text",.inform=T) %>% unlist %>% mod(1) %>%  round(.,digits=2) %>% unique %>% sort
vals.mod4 = scan("SourceData/Other/IRI.mod4_values.txt")
