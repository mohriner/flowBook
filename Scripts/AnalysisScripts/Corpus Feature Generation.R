source("Scripts/loadLibraries.r")
source("Scripts/AnalysisScripts/CentralPillarFunctions.r")
source("Scripts/AnalysisScripts/GroovinessFeatureFunctions.R")
meta = read.delim("SourceData/verseMetadata.txt")
load(file = "DerivedData/FlowBookCorpus.rdata")
source("Scripts/AnalysisScripts/RhymeEntropyFunction.r")
load(file = "DerivedData/CMU_contours.rdata")
segments = fread("DerivedData/corpus_groove_segments.txt",header=T)
corpora = tbl_df(corpora)

# Make sure it's descending corpus for kendrick lamar, since k is after g whereas
# b (black thought) is before. (In the arrange line below)
titles = corpora %>% 
  filter(!(corpus %in% c("didactic"))) %>% 
  arrange(corpus,as.character(verse)) %>% 
  .$verse %>% unique %>% as.character

getSylsPerWord = function(verseTitle){
  w = corpora %>% 
    filter(verse==verseTitle) %>% 
    .$word %>% as.character
  w = w[cumsum(rle(w)$lengths)]
  m = match(toupper(w),CMU_contours$word)
  return(mean(CMU_contours$n.syl[m]))
}

corpora %>% 
  filter(!(corpus %in% c("didactic"))) %>% 
  group_by(corpus,verse) %>% 
  slice(1) %>% 
  group_by(corpus) %>% 
  summarize(n=length(word)) -> corpus.counts

features = data.frame(corpus = rep(corpus.counts$corpus,corpus.counts$n),
                      verse = as.character(titles),
                      sylsPerWord = laply(titles,getSylsPerWord)) %>% 
  arrange(corpus,verse)
remove("getSylsPerWord","CMU_contours")

# Feature 2: Syllables per second ---------------------------------

features = features %>% mutate(
  sylsPerSecond = 
    corpora %>% 
    mutate(tempo = meta$bpm[match(corpora$verse,meta$title)]) %>% 
    filter(lineEnding==0,breathEnding==0,!(corpus %in% c("didactic"))) %>% # Excludes long syllables
    mutate(sec = (60/tempo/4) * (16/quant) *duration) %>%
    group_by(corpus,verse) %>% 
    summarize(rate = 1/mean(sec)) %>% 
    arrange(corpus,as.character(verse)) %>% 
    .$rate
)

# Endnote 4: Insignificance of region and era on delivery rate ----------------

features = features %>% 
  mutate(tempo = meta$bpm[match(features$verse,meta$title)],
         region = meta$region[match(features$verse,meta$title)],
         era = meta$era[match(features$verse,meta$title)],
         year = meta$songYear[match(features$verse,meta$title)])
features = features %>% select(corpus,verse,region,era,year,tempo,sylsPerWord,sylsPerSecond)

# Saturation ------------------------------------------------

features = features %>% 
  mutate(saturation = corpora %>% 
           filter(!(corpus %in% c("didactic"))) %>% # genre-wide corpus
           mutate(bi2 = round((beatIndex * 4)) / 4) %>% # conflates sub-sixteenths
           group_by(corpus,verse,bi2) %>% slice(1) %>% ungroup %>% # remove all but one per sixteenth
           mutate(measure = beatIndex %/% 4) %>%  # get measure
           group_by(corpus,verse,measure) %>% 
           summarize(n = length(syllable)) %>% # count syls per measure
           group_by(corpus,verse) %>% 
           summarize(sat = mean(n)) %>% #average syls per measure.
           arrange(corpus,as.character(verse)) %>% .$sat
  )

# Endnote 7: Change in saturation --------------------------------------

features = features %>% 
  mutate(sat.delta = corpora %>% 
           filter(!(corpus %in% c("didactic"))) %>% # genre-wide corpus
           mutate(bi2 = round((beatIndex * 4)) / 4) %>% # conflates sub-sixteenths
           group_by(corpus,verse,bi2) %>% slice(1) %>% ungroup %>% # remove all but one per sixteenth
           mutate(measure = beatIndex %/% 4) %>%  # get measure
           group_by(corpus,verse,measure) %>% 
           summarize(n = length(syllable)) %>% 
           group_by(corpus,verse) %>% 
           summarize(delta = mean(abs(diff(n)))) %>% 
           arrange(corpus,as.character(verse)) %>%
           .$delta
  )

# Endnote 11: Rhyme mixture --------------------------------------------

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

getRhymeMixture = function(v){
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
mix = laply(titles,getRhymeMixture,.inform=T);
features = features %>% mutate(
  rhymeMixture = mix
)
remove(list=c("mix","countTranspositions","getRhymeMixture"))


# Endnote 14: Rhyme Entropy ---------------------------------------------------

x = laply(titles,getVerseEntropy,row.index = NA,mod4 = F,.inform=T,.progress="text")
features = features %>% mutate(
  rhymeEntropy = x
)


# Endnote 15: mod4 rhymeEntropy ----------------------------------------

x = laply(titles,getVerseEntropy,row.index = NA,mod4 = T,.inform=T,.progress="text")
features = features %>% mutate(
  rhymeEntropy4 = x
)

# Endnote 16: Grooviness ----------------------------------------------

x = laply(titles,GetGrooviness,.progress="text",.inform=T)
features = features %>% mutate(
  grooviness = x
)


# Endnote 17: Groove Adherence ----------------------------------------
x = laply(titles,GetGrooveAdherence,.progress="text")
features = features %>% mutate(
  adherence = x
)

# Endnote 29: Other features -------------------------------------------

getPhraseLength = function(verseTitle){
  corpora %>% 
    mutate(beatLength = round(60/meta$bpm[match(verse,meta$title)],digits=2)) %>%
    filter(verse==verseTitle,
           breathEnding==1) %>%
    summarize(l = round(mean(diff(beatIndex))*unique(beatLength),digits=2)) %>% 
    .$l
}
getPhraseLengthVariance = function(verseTitle){
  corpora %>% 
    filter(verse==verseTitle) %>% # genre-wide corpus
    group_by(phrase) %>% 
    summarize(l = diff(range(beatIndex))) %>% 
    summarize(v = var(l)) %>% 
    .$v
}
getRhymeLength.instances = function(verseTitle){
  corpora %>% 
    filter(verse==verseTitle,rhymeIndex==1) %>% # genre-wide corpus
    group_by(rhymeClass) %>% 
    summarize(n=length(word)) %>% 
    summarize(l = round(mean(n),digits=2)) %>% 
    .$l
}
getRhymeLength.syllables = function(verseTitle){
  corpora %>% 
    filter(verse==verseTitle,!is.na(rhymeClass)) %>% 
    group_by(rhymeClass) %>% 
    summarize(l=max(rhymeIndex)) %>% 
    summarize(n = round(mean(l),digits=2)) %>% 
    .$n
}
getDupleness = function(verseTitle){
  n = segments %>% 
    filter(verse==verseTitle,effort.rate==0) %>% 
    .$end %>% max
  d = segments %>% 
    filter(verse==verseTitle,effort.rate==0,class=="22222222") %>% 
    .$length %>% sum
  round(d/n,digits=2)
}
features = features %>% mutate(
  phraseLength = laply(titles,getPhraseLength),
  phraseLengthVariance = laply(titles,getPhraseLengthVariance),
  instancesPerClass = laply(titles,getRhymeLength.instances),
  syllablesPerInstance = laply(titles,getRhymeLength.syllables),
  dupleness = laply(titles,getDupleness)
)
write.table(features,"DerivedData/CorpusGlobalFeatures.txt",quote=F,row.names=F,sep="\t")

features %>% View
# t-tests of features
features2 = features %>% 
  filter(corpus %in% c("genre1","kendrickLamar")) %>% 
  select(saturation,sat.delta,phraseLength,phraseLengthVariance,instancesPerClass,
         syllablesPerInstance,rhymeMixture,dupleness,sylsPerSecond,sylsPerWord,
         rhymeEntropy,rhymeEntropy4,grooviness,adherence,tempo) %>% 
  as.matrix

Table5.2 = apply(features2,2,function(i){
  a = i[1:30]
  b = i[31:105]
  t = t.test(a,b)
  c(mean(a),sd(a),mean(b),sd(b),mean(a)-mean(b),t$statistic,t$p.value) %>% unname
}) %>% round(digits=2) %>% t
colnames(Table5.2) = c("BT.mean","BT.sd","RM.mean","RM.sd","MeanDiff","t","p")
write.table(Table5.2,file="Examples/Chapter 5/Table 5.2.txt",quote=F,row.names=T)
#
Table5.2.Kendrick = apply(features2,2,function(i){
  a = i[76:104]
  b = i[1:75]
  t = t.test(a,b)
  c(mean(a),sd(a),mean(b),sd(b),mean(a)-mean(b),t$statistic,t$p.value) %>% unname
}) %>% round(digits=2) %>% t
colnames(Table5.2.Kendrick) = c("KL.mean","KL.sd","RM.mean","RM.sd","MeanDiff","t","p")
write.table(Table5.2.Kendrick,file="~/Desktop/Table 5.2.Kendrick.txt",quote=F,row.names=T)

features = read.delim("DerivedData/CorpusGlobalFeatures.txt")

# Endnote 31: Black Thought tracks with features of virtuosity ------------
aboveAverage = apply(features[,6:20],2,function(i){i >= mean(i[31:105])})
rownames(aboveAverage) = features$verse
x = aboveAverage[,c("sylsPerWord","sylsPerSecond","saturation",
                "sat.delta","instancesPerClass","syllablesPerInstance",
                "dupleness","rhymeMixture")] %>% rowSums %>% 
  is_greater_than(3) %>% which %>% names
a = (length(x[1:13])/30) # BT
b = (length(x[14:26])/75) # The genre

# 
features %>% 
  filter(corpus %in% c("genre1","kendrickLamar")) %>% 
  arrange(corpus) %>% 
  group_by(verse) %>% 
  slice(1) %>% 
  ungroup %>% 
  .$verse


# MANOVA ------------------------------------------------------------------

features = read.delim("DerivedData/CorpusGlobalFeatures.txt")
features %>% View
features %>% names
e_manova = manova(cbind(tempo,sylsPerWord,sylsPerSecond,saturation,sat.delta,
                        rhymeMixture,rhymeEntropy,rhymeEntropy4,grooviness,
                        adherence,phraseLength,phraseLengthVariance,
                        instancesPerClass,syllablesPerInstance,dupleness)~as.factor(corpus),
                  data=features)
summary(e_manova)
fit = aov(sylsPerWord~corpus,data=features)
quartz()
plot(fit)
interaction.plot(e_manova)
library(gplots)
attach(features)
corpus = factor(corpus)
plotmeans(tempo~saturation)
features %>% names

# Correlation of features in genre1
install.packages("ellipse")
library(ellipse)
features %>% filter(corpus=="genre1") %>% 
  select(-corpus,-verse,-era,-region) %>% cor -> corr.features
ord <- order(corr.features[1,])
xc <- corr.features[ord, ord]
colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
quartz()
plotcorr(xc, col=colors[5*xc + 6], type = "lower",diag=T)
