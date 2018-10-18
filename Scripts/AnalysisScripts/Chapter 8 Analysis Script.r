source("Scripts/loadLibraries.r")
source("Scripts/DataScrubbingScripts/AppendContinuousOnsets.r")
source("Scripts/AnalysisScripts/NonAlignmentFunctions.r")
meta = read.delim("SourceData/verseMetadata.txt",header=T)

GrooveSections = list(53:67,96:111,197:211)

llply(GrooveSections,function(s){
  bi.c = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$c.bi
  bi.q = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$beatIndex
  optimize.alignment(bi.c,bi.q,check.phase = T,check.swing = T,check.tempo = T)
})

# Endnote 4: Repeated correction of “Don’t Let Up" --------------------

source("Scripts/loadLibraries.r")
vowels = c("AA","AE","AH","AO","AW","AY","EH","ER","EY","IH","IY","OW","OY","UH","UW")
vowels = sort(c(paste(vowels,"0",sep=""),paste(vowels,"1",sep=""),paste(vowels,"2",sep="")))

TextGrid2Onset = function(TG){
  s1 = grep("item \\[1\\]",TG)
  s2 = grep("item \\[2\\]",TG)
  
  # Lines with bracketed numbers
  ints = grep("\\[[[:digit:]]",TG)
  l1 = ints[which(ints>s1&ints<s2)]
  l2 = ints[which(ints>s2)]
  on1 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+1]))
  off1 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+2]))
  lab1 = gsub("[(text\\=[:space:]\")]","",TG[l1+3])
  on2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+1]))
  off2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+2]))
  lab2 = gsub("[(text\\=[:space:]\")]","",TG[l2+3])
  
  ph.matrix = data.frame(on1,off1,lab1)
  w.matrix = data.frame(on2,off2,lab2)
  # Abbreviate the two matrices
  ph.matrix = ph.matrix[-which(ph.matrix$lab1=="sp"),]
  w.matrix = w.matrix[-which(w.matrix$lab2=="sp"),]
  if(any(w.matrix$lab2=="")){w.matrix = w.matrix[-which(w.matrix$lab2==""),]}
  ph.matrix
}

TG1 = readLines("SourceData/PhoneticData/flowBeatAlignment/CorrectedTextGrids/talibKweli_moodswing1.TextGrid")
TG2 = readLines("SourceData/Other/talibKweli_moodswing1_recheck.TextGrid")

a = TextGrid2Onset(TG1)
b = TextGrid2Onset(TG2)
d = a$on1 - b$on1
v = a$lab1 %>% as.character %>% nchar(.) %>% equals(3) %>% which # vowel indexes
cons = v = a$lab1 %>% as.character %>% nchar(.) %>% equals(3) %>% not %>% which # vowel indexes
mean(d==0) # 55% unchanged.
d[v] %>% abs %>% mean # Mean error of 5ms for vowels...
d[cons] %>% abs %>% mean # ...and 11 ms for consonants


# Endnote 9: phase adjustment in Kweli vs. the corpus --------------------
source("Scripts/DataScrubbingScripts/AppendContinuousOnsets.r")
source("Scripts/AnalysisScripts/NonAlignmentFunctions.r")
corpora = corpora %>% mutate(measure = beatIndex %/% 4)
verses = corpora %>% filter(!is.na(c.bi),verse!="cream") %>% .$verse %>% as.character %>% unique
#
GetMeanPhase = function(verseTitle){
  first.m = corpora %>% filter(verse==verseTitle) %>% .$measure %>% min
  if(first.m==-1){first.m = 0}
  last.m = corpora %>% filter(verse==verseTitle,beatIndex%%4>2) %>% .$measure %>% max
  ph = laply(first.m:last.m,function(i){
    corpora %>% filter(verse==verseTitle,measure==i) %>% .$c.bi -> bi.c
    corpora %>% filter(verse==verseTitle,measure==i) %>% .$beatIndex -> bi.q
    OptimizeAlignment(bi.q,bi.c,T,F,F)$phase
  },.inform=T)
  mean(ph,na.rm=T)
}
meanPhaseByMeasure = laply(verses,GetMeanPhase,.inform=T,.progress="text")
corpora = corpora %>% mutate(tempo = meta$bpm[match(corpora$verse,meta$title)])
names(meanPhaseByMeasure) = verses
meanPhaseByMeasure.seconds = meanPhaseByMeasure
for(i in 1:length(meanPhaseByMeasure)){
  bpm = meta$bpm[match(names(meanPhaseByMeasure[i]),meta$title)]
  meanPhaseByMeasure.seconds[i] = ((60/bpm)/60*meanPhaseByMeasure[i])
}
# t-test for seconds (i.e., "clock time"), reported in footnote.
corp = corpora %>% filter(!is.na(c.bi)) %>% distinct(corpus,verse)
a = meanPhaseByMeasure.seconds[which(corp$corpus[match(verses,corp$verse)]=="genre1")]
b = meanPhaseByMeasure.seconds[which(corp$corpus[match(verses,corp$verse)]=="talibKweli")]
l = list(paste("(M = ",round(mean(a),digits=2),", SD = ",round(sd(a),digits=2),")",sep=""),
         paste("(M = ",round(mean(b),digits=2),", SD = ",round(sd(b),digits=2),")",sep=""),
         paste("t(",round(t.test(a,b)$parameter),") = ",
               round(t.test(a,b)$statistic,digits=2), ", p < ",
               round(t.test(a,b)$p.value,digits=3),
               sep=""))
l

# t-test for minutes (sixtieth of a beat, i.e., "musical time"), reported in text.
a = meanPhaseByMeasure[which(corp$corpus[match(verses,corp$verse)]=="genre1")]
b = meanPhaseByMeasure[which(corp$corpus[match(verses,corp$verse)]=="talibKweli")]
l = list(paste("(M = ",round(mean(a),digits=2),", SD = ",round(sd(a),digits=2),")",sep=""),
         paste("(M = ",round(mean(b),digits=2),", SD = ",round(sd(b),digits=2),")",sep=""),
         paste("t(",round(t.test(a,b)$parameter),") = ",
               round(t.test(a,b)$statistic,digits=2), ", p < ",
               round(t.test(a,b)$p.value,digits=3),
               sep=""))
l
# quartz(width=3,height=1.5)
# par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
#     family="Times New Roman")
# x = density(-b)
# x$call=""
# plot(x,xlab="Minutes behind Beat",xlim=c(-5,20),ylim=c(0,.15),lwd=2)
# y = density(-a)
# y$call=""
# lines(y,lty=2,lwd=.5)
# legend(x = -5,y = .15,legend = c("Kweli","Rap Music"),
#        lty=1:2,lwd = c(2,.5),
#        bty="n",y.intersp = .75)
# plot(density(a))
# lines(density(b))
# names(corpora)
# #

# Endnote 16: use of swing by Kweli ---------------------------------------


GetSwing = function(verseTitle){
  first.m = corpora %>% filter(verse==verseTitle) %>% .$measure %>% min
  if(first.m==-1){first.m = 0}
  last.m = corpora %>% filter(verse==verseTitle,beatIndex%%4>2) %>% .$measure %>% max
  sw = laply(first.m:last.m,function(i){
    corpora %>% filter(verse==verseTitle,measure==i) %>% .$c.bi -> bi.c
    corpora %>% filter(verse==verseTitle,measure==i) %>% .$beatIndex -> bi.q
    OptimizeAlignment(bi.q,bi.c,T,T,F)$swing
  },.inform=T)
  sw
}
swingByMeasure = llply(verses,GetSwing,.inform=T,.progress="text")
corpora = corpora %>% mutate(tempo = meta$bpm[match(corpora$verse,meta$title)])
names(swingByMeasure) = verses
# How many measure have no swing?
swingByMeasure[1:12] %>% unlist %>% equals(1) %>% mean # genre
swingByMeasure[13:42] %>% unlist %>% equals(1) %>% mean # Kweli
# Does tempo affect swing in the corpus?
tempo = meta[match(verses,meta$title),"bpm"]
a = tempo[1:12]
b = laply(swingByMeasure[1:12],mean)
lm(b~a) %>% summary # no.

# In Kweli?
tempo = meta[match(verses,meta$title),"bpm"]
a = tempo[13:42]
b = laply(swingByMeasure[13:42],mean)
lm(b~a) %>% summary # no.
a = swingByMeasure %>% .[1:12] %>% unlist # genre
b = swingByMeasure %>% .[13:42] %>% unlist # kweli
t.test(a,b) # Not significantly different.

# Endnote 16: Demonstration that adjustments work as advertized -----------------------

bi.q = seq(0,1,.25)
bi.q %>% # "Straight sixteenth notes"
  multiply_by(1.01) %>% #...performed 1% faster
  add(1/60) %>% #...performed 1 minute late
  add(c(0,1/12,0,1/12,0)) -> bi.c # with a swing ratio of 2:1
# This sequence of five notes has 16.5 minutes of lateness, or an average
# non-alignment of 3.3. The "performed 1 minute late" is 5 minutes of lateness;
# each of the swung notes adds another five [(1/12)/(1/60) = 5]. The other 
# 1.5 minutes of lateness is from the "performed 1% faster":
bi.q %>% # 
  multiply_by(1.01) %>% # performed 1% faster
  subtract(seq(0,1,.25)) %>% # subtracted from the original
  divide_by(1/60) %>% sum # sum of minutes = 1.5
# So this command should return 3.3:
GetNonalignment(bi.q,
                bi.c,
                phase.adj = 0, 
                swing.adj = 1, 
                tempo.adj = 1) %>% equals(3.3)
# And this one should return 0:
GetNonalignment(bi.q,
                bi.c,
                phase.adj = -1, 
                swing.adj = 2, 
                tempo.adj = 1.01) %>% equals(0)


# Endnote 18: Tempo shift in Kweli vs. corpus -----------------------------------------

load("DerivedData/NonAlignmentFeatures.rdata")
nonalign.meas %>% filter(corpus=="talibKweli") %>% .$tempo ->a
nonalign.meas %>% filter(corpus=="genre1") %>% .$tempo ->b
l = list(paste("(M = ",round(mean(a),digits=2),", SD = ",round(sd(a),digits=2),")",sep=""),
         paste("(M = ",round(mean(b),digits=2),", SD = ",round(sd(b),digits=2),")",sep=""),
         paste("t(",round(t.test(a,b)$parameter),") = ",
               round(t.test(a,b)$statistic,digits=2), ", p < ",
               round(t.test(a,b)$p.value,digits=3),
               sep=""))
l

# Endnote 19: Kweli’s saturation ------------------------------------------------------

corpora %>% 
  filter(corpus == "talibKweli") %>% # genre-wide corpus
  mutate(bi2 = round((beatIndex * 4)) / 4) %>% # conflates sub-sixteenths
  group_by(verse,bi2) %>% slice(1) %>% ungroup %>% # remove all but one per sixteenth
  mutate(measure = beatIndex %/% 4) %>%  # get measure
  group_by(verse,measure) %>% 
  summarize(n = length(syllable)) %>% # count syls per measure
  .$n -> a
corpora %>% 
  filter(corpus == "genre1") %>% # genre-wide corpus
  mutate(bi2 = round((beatIndex * 4)) / 4) %>% # conflates sub-sixteenths
  group_by(verse,bi2) %>% slice(1) %>% ungroup %>% # remove all but one per sixteenth
  mutate(measure = beatIndex %/% 4) %>%  # get measure
  group_by(verse,measure) %>% 
  summarize(n = length(syllable)) %>% # count syls per measure
  .$n -> b
l = list(paste("(M = ",round(mean(a),digits=2),", SD = ",round(sd(a),digits=2),")",sep=""),
         paste("(M = ",round(mean(b),digits=2),", SD = ",round(sd(b),digits=2),")",sep=""),
         paste("t(",round(t.test(a,b)$parameter),") = ",
               round(t.test(a,b)$statistic,digits=2), ", p < ",
               round(t.test(a,b)$p.value,digits=3),
               sep=""))
l
# Endnote 24: Kweli’s use of deceleration, contextualized -----------------------------

# Determine phrases that end with n consecutive quantized sixteenth notes.
num = 4
slopes = corpora %>% 
  filter(!is.na(c.bi)) %>% # use only verses that have continuous data
  group_by(verse) %>% 
  mutate(dur.quant = c(diff(beatIndex),NA), # duration in beats
         dur.cont = c(diff(c.bi),NA) # duration in syllables
  ) %>% 
  group_by(corpus,verse,phrase) %>% 
  mutate(num.syl = n(), index = 1:n()) %>% # count number of syllables
  filter(num.syl>(num+1)) %>% # discard phrases with less than n syllables
  slice((n()-num):(n()-1)) %>% # discard all but the last n penultimate syllables
  mutate(all1pos = all(dur.quant==.25)) %>% # determine if all remaining syllables are .25 beats
  filter(all1pos==T) %>% # discard phrases in which they aren't
  summarize(sl = lm(dur.cont~beatIndex)$coefficients %>% .[2] %>% unname) # extract the slope from a linear regression
slopes %>% filter(corpus=="genre1") %>% .$sl -> a
slopes %>% filter(corpus=="talibKweli") %>% .$sl -> b
l = list(paste("(M = ",round(mean(a),digits=2),", SD = ",round(sd(a),digits=2),")",sep=""),
         paste("(M = ",round(mean(b),digits=2),", SD = ",round(sd(b),digits=2),")",sep=""),
         paste("t(",round(t.test(a,b)$parameter),") = ",
               round(t.test(a,b)$statistic,digits=2), ", p < ",
               round(t.test(a,b)$p.value,digits=3),
               sep=""))
l
# Endnote 25: Kweli has greater unadjusted non-alignment ------------------------------

load("DerivedData/NonAlignmentFeatures.rdata")
nonalign.meas %>% filter(corpus=="talibKweli") %>% .$async1 -> a
nonalign.meas %>% filter(corpus=="genre1") %>% .$async1 -> b
l = list(paste("(M = ",round(mean(a),digits=2),", SD = ",round(sd(a),digits=2),")",sep=""),
         paste("(M = ",round(mean(b),digits=2),", SD = ",round(sd(b),digits=2),")",sep=""),
         paste("t(",round(t.test(a,b)$parameter),") = ",
               round(t.test(a,b)$statistic,digits=2), ", p < ",
               round(t.test(a,b)$p.value,digits=3),
               sep=""))
l