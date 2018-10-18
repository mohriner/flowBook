
# Feature generation functions --------------------------------------------
# 1. GetCorpus
GetCorpus = function(verseTitle){
  match(verseTitle,meta$title) %>% meta$tranche[.] %>% as.character %>% return
}

# 2. verse, same as titles

# 3. region 
GetRegion = function(verseTitle){
  match(verseTitle,meta$title) %>% meta$region[.] %>% as.character %>% return
}

# 4. year 
GetYear = function(verseTitle){
  match(verseTitle,meta$title) %>% meta$songYear[.] %>% as.integer %>% return
}

# 5. tempo 
GetTempo = function(verseTitle){
  match(verseTitle,meta$title) %>% meta$bpm[.] %>% as.numeric %>% return
}

# 6.
GetSylsPerWord = function(verseTitle){
  w = corpora %>% 
    filter(verse==verseTitle) %>% 
    .$word %>% as.character
  w = w[cumsum(rle(w)$lengths)]
  m = match(toupper(w),CMU_contours$word)
  return(mean(CMU_contours$n.syl[m]))
}

# 7.
GetSylsPerSecond = function(verseTitle){
  corpora %>% 
    filter(verse==verseTitle,lineEnding==0,breathEnding==0) %>% 
    mutate(tempo = meta$bpm[match(verseTitle,meta$title)]) %>% 
    mutate(sec = (60/tempo/4) * (16/quant) *duration) %>%
    group_by(corpus,verse) %>% 
    summarize(rate = 1/mean(sec)) %>% 
    .$rate %>% 
    return
}

# 8.
GetSaturation = function(verseTitle){
  corpora %>% 
    filter(verse==verseTitle) %>% # genre-wide corpus
    mutate(bi2 = round((beatIndex * 4)) / 4) %>% # conflates sub-sixteenths
    group_by(bi2) %>% slice(1) %>% ungroup %>% # remove all but one per sixteenth
    mutate(measure = beatIndex %/% 4) %>%  # get measure
    group_by(measure) %>% 
    summarize(n = length(syllable)) %>% # count syls per measure
    slice(2:(dim(.)[1]-1)) %>% 
    summarize(sat = mean(n)) %>% #average syls per measure.
    .$sat %>% 
    return
}

# 9.
GetSaturationDelta = function(verseTitle){
  corpora %>% 
    filter(verse==verseTitle) %>% # genre-wide corpus
    mutate(bi2 = round((beatIndex * 4)) / 4) %>% # conflates sub-sixteenths
    group_by(bi2) %>% slice(1) %>% ungroup %>% # remove all but one per sixteenth
    mutate(measure = beatIndex %/% 4) %>%  # get measure
    group_by(measure) %>% 
    summarize(n = length(syllable)) %>% 
    slice(2:(dim(.)[1]-1)) %>% 
    summarize(delta = mean(abs(diff(n)))) %>% 
    .$delta
}

# 10. Rhyme mixture
countTranspositions <- function(r.index){
  if(length(r.index)==1){return(0)}
  count = 0
  for (j in 2:length(r.index)) {
    key = r.index[j] 
    i = j - 1 
    while (i > 0 && r.index[i] > key) {
      r.index[(i + 1)] = r.index[i]
      i = i - 1 
      count = count + 1
    }
    r.index[(i + 1)] = key
  }
  count
} 

GetRhymeMixture = function(v){
  r.index = corpora %>% filter(verse==v) %>% 
    .$rhymeClass
  r.index = r.index[!is.na(r.index)]
  if(length(r.index)==0){return(NA)}
  r.index = rollapply(r.index,2,function(i) i[1]!=i[2]) %>% c(TRUE,.) %>% which %>% r.index[.]
  trans = countTranspositions(r.index)
  n.meas = corpora %>% filter(verse==v) %>% .$beatIndex %>% max %>% divide_by_int(4)
  mixture = trans %>% divide_by(n.meas) %>% round(.,digits=2)
  mixture
}

# 11. and 12. (rhyme entropy)--functions in RhymeEntropyFunction.r

# 13. Grooviness, function in GroovinessFeatureFunctions.R, GetGrooviness

# 14. Adherence, function in GroovinessFeatureFunctions.R, GetGrooveAdherence

# 15.
GetPhraseLength = function(verseTitle){
  corpora %>% 
    mutate(beatLength = round(60/meta$bpm[match(verse,meta$title)],digits=2)) %>%
    filter(verse==verseTitle,
           breathEnding==1) -> f1
  if(dim(f1)[1]>0){
    f1 %>%
      summarize(l = round(mean(diff(beatIndex))*unique(beatLength),digits=2)) %>% 
      .$l %>% 
      return()
  }else{
    return(NA)
  }
}

# 16.
GetPhraseLengthVariance = function(verseTitle){
  corpora %>% 
    filter(verse==verseTitle) %>% # genre-wide corpus
    group_by(phrase) %>% 
    summarize(l = diff(range(beatIndex))) %>% 
    summarize(v = var(l)) %>% 
    .$v
}

# 17.
GetRhymeLength.instances = function(verseTitle){
  corpora %>% 
    filter(verse==verseTitle,rhymeIndex==1) %>% # genre-wide corpus
    group_by(rhymeClass) %>% 
    summarize(n=length(word)) %>% 
    summarize(l = round(mean(n),digits=2)) %>% 
    .$l
}

# 18.
GetRhymeLength.syllables = function(verseTitle){
  corpora %>% 
    filter(verse==verseTitle,!is.na(rhymeClass)) %>% 
    group_by(rhymeClass) %>% 
    summarize(l=max(rhymeIndex)) %>% 
    summarize(n = round(mean(l),digits=2)) %>% 
    .$n
}

# 19.
GetDupleness = function(verseTitle){
  n = segments %>% 
    filter(verse==verseTitle,effort.rate==0) %>% 
    .$end %>% max
  d = segments %>% 
    filter(verse==verseTitle,effort.rate==0,class=="22222222") %>% 
    .$length %>% sum
  round(d/n,digits=2)
}

# 20.
source("Scripts/DataScrubbingScripts/GetL3Durations.R")
GetMeanCor = function(verseTitle){
  GetL3RhythmVector(verseTitle,m.range=NA) -> r
  r = r[1:((length(r) %/% 16) *16)]
  matrix(r,length(r)/16,16,byrow=T) -> m
  laply(1:(dim(m)[1]-2),function(i){
    round(cor(m[i,],m[i+1,]),digits=2)
  })  %>% mean(na.rm=T) %>% round(.,digits=2) %>% return
}

# 21. 
GetMeanCor.Nol3 = function(verseTitle){
  GetRhythmVectors(verseTitle,m.range)$a -> r
  r = r[1:((length(r) %/% 16) *16)]
  matrix(r,length(r)/16,16,byrow=T) -> m
  laply(1:(dim(m)[1]-2),function(i){
    round(cor(m[i,],m[i+1,]),digits=2)
  }) %>% mean(na.rm=T) %>% round(.,digits=2) %>% return
}
