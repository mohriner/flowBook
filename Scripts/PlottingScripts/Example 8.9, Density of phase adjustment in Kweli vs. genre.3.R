source("Scripts/loadLibraries.r")
source("Scripts/DataScrubbingScripts/AppendContinuousOnsets.r")
source("Scripts/AnalysisScripts/NonAlignmentFunctions.r")
meta = read.delim("SourceData/verseMetadata.txt",header=T)


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
corpora = corpora %>% mutate(measure = beatIndex %/% 4)
verses = corpora %>% filter(!is.na(c.bi),verse!="cream") %>% .$verse %>% as.character %>% unique
meanPhaseByMeasure = laply(verses,GetMeanPhase,.inform=T,.progress="text")
corpora = corpora %>% mutate(tempo = meta$bpm[match(corpora$verse,meta$title)])
names(meanPhaseByMeasure) = verses
meanPhaseByMeasure.seconds = meanPhaseByMeasure
for(i in 1:length(meanPhaseByMeasure)){
  bpm = corpora %>% filter(verse==names(meanPhaseByMeasure[i])) %>% .$tempo %>% unique
  meanPhaseByMeasure.seconds[i] = ((60/bpm)/60*meanPhaseByMeasure[i])
}
# t-test for seconds (i.e., "clock time"), reported in footnote.
corp = corpora %>% filter(!is.na(c.bi)) %>% distinct(corpus,verse)
a = meanPhaseByMeasure.seconds[which(corp$corpus[match(verses,corp$verse)]=="genre1")]
b = meanPhaseByMeasure.seconds[which(corp$corpus[match(verses,corp$verse)]=="talibKweli")]
t.test(a,b)

# t-test for minutes (sixtieth of a beat, i.e., "musical time"), reported in text.
a = meanPhaseByMeasure[which(corp$corpus[match(verses,corp$verse)]=="genre1")]
b = meanPhaseByMeasure[which(corp$corpus[match(verses,corp$verse)]=="talibKweli")]
t.test(a,b)
quartz(width=3,height=1.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
x = density(-b)
x$call=""
plot(x,xlab="Minutes behind Beat",xlim=c(-5,20),ylim=c(0,.15),lwd=2)
y = density(-a)
y$call=""
lines(y,lty=2,lwd=.5)
legend(x = -5,y = .15,legend = c("Kweli","Rap Music"),
       lty=1:2,lwd = c(2,.5),
       bty="n",y.intersp = .75)


x1 = -meanPhaseByMeasure["getby1"] %>% unname
y1 = x$y[which.min(abs(x$x - x1))]
points(x1,y1,pch=20,cex=1.5)
text(x1,y1,"Get By",pos=3)
quartz.save(file="Examples/Chapter 8/Example 8.9.pdf",type="pdf");dev.off()
remove(list = c("bpm","x1","y1","a","b","i","i1","meanPhaseByMeasure","meanPhaseByMeasure.seconds","verses","x","y","GetMeanPhase","corp"))
