source("Scripts/loadLibraries.r")
source("Scripts/PlottingScripts/PlotVerse.r")
source("Scripts/PlottingScripts/PlotGrooveSegmentation.r")
source("Scripts/AnalysisScripts/CentralPillarFunctions.r")
load("DerivedData/FlowBookCorpus.rdata")
source("Scripts/AnalysisScripts/GroovinessFeatureFunctions.R")
meta = read.delim("SourceData/verseMetadata.txt",header=T)
features = read.delim("DerivedData/CorpusGlobalFeatures.txt")
segments = read.delim("DerivedData/corpus_groove_segments.txt")


# Endnote 4: Mean IAI in Black Thought verses -----------------------------
titles = corpora %>% filter(corpus=="blackThought") %>% .$verse %>% unique
mean.IAIs = laply(titles,function(v){
  GetL3Durations(v) %>% strsplit(.,"") %>% unlist %>% as.numeric %>% 
    mean
}); names(mean.IAIs) = titles
m = GetL3RhythmVector("theOtherSide1",m.range=NA)[1:128] %>% equals(1) %>% which %>% 
  diff %>% mean
sort(mean.IAIs)

# Endnote 5: “The OtherSide” is the fifth longest duple span -------------

l = segments %>% filter(verse=="theOtherSide1",effort.rate == .0625) %>% 
  slice(1) %>% .$length
segments %>% 
  filter(corpus=="blackThought",
         class=="22222222",
         effort.rate==.0625) %>%
  arrange(desc(length)) %>% 
  select(verse,start,length) %>% 
  filter(length>=l) # OK, it's tied for fifth

# Endnote 6 (Now or Never has the most grooves in the first four m --------

llply(c(0,0.0625,.125,.25),function(i){
  v = segments %>% 
    filter(corpus=="blackThought",effort.rate==i,start<=64) %>% 
    group_by(verse) %>% summarize(n=length(start)) %>% 
    arrange(desc(n)) %>% 
    mutate(m = n==max(n)) %>% 
    filter(m==T) %>% 
    .$verse
})

# Endnote 7 (correlation of percentage of 1mod2 accents and number of grooves----


# Correlation of number of grooves in the first four measures at
# effort rate of .0625 vs. the percentage of accents in those measures
# that are on 1mod2 positions (i.e., not on the beat or at a beat's
# midpoint).
titles = corpora %>% filter(corpus=="blackThought") %>% .$verse %>% unique %>% as.character
offbeatAccents = laply(titles,function(v){
  o = corpora %>% filter(verse==v,accent==1,beatIndex<16,beatIndex%%.5 %in% c(0,.25)) %>% 
    group_by(mod2 = beatIndex%%.5) %>% 
    summarize(n = length(word)) %>% 
    mutate(p = n/sum(n)) %>% 
    filter(mod2==.25) %>% 
    .$p
  if(length(o)==0){o=0}
  o
},.inform=T)
names(offbeatAccents) = titles

n.grooves = laply(titles,function(v){
  o = segments %>% filter(verse==v, start<=64,effort.rate == .0625) %>% 
    summarize(n = length(start)) %>% 
    .$n
  if(length(o)==0){o=0}
  o
},.inform=T)
names(n.grooves) = titles
lm(n.grooves~offbeatAccents) %>% summary # r2 = .25, p<.002

# Endnote 9: "How I Got Over" is the among the grooviest openings.
segments %>% 
  filter(verse=="howIGotOver",effort.rate==0,start<120)
segments %>% 
    filter(corpus=="blackThought",effort.rate==0,start<120) %>% 
    group_by(verse) %>% summarize(n=length(start)) %>% 
    arrange((n))

# Endnote 10: Length of initial grooves for an adaptive listener ----
l = segments %>% filter(verse=="howIGotOver",effort.rate == 0) %>% 
  slice(1) %>% .$length
segments %>% 
  filter(corpus=="blackThought") %>% 
  group_by(verse) %>% slice(1) %>% ungroup %>% 
  arrange(desc(length)) %>% 
  select(verse,start,length) %>% 
  filter(length>=l) # OK, it's tied for fifth
# Endnote 11: Rarity of more than 4 consecutive 3s -------------------------

titles = corpora %>% .$verse %>% as.character %>% unique
x = laply(titles,GetL3Durations,.progress="text")
y = llply(x,function(i){
  rle1 = i %>% strsplit(.,"") %>% unlist %>% rle
  rle1 = rle1$lengths[which(rle1$values=="3")]
  rle1
})
titles[which(laply(y,function(i) 10 %in% i))]