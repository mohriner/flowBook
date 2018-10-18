source("Scripts/loadLibraries.r")
source("Scripts/PlottingScripts/PlotVerse.r")
source("Scripts/PlottingScripts/PlotGrooveSegmentation.r")
source("Scripts/AnalysisScripts/CentralPillarFunctions.r")
load("DerivedData/FlowBookCorpus.rdata")
source("Scripts/AnalysisScripts/GroovinessFeatureFunctions.R")
meta = read.delim("SourceData/verseMetadata.txt",header=T)
features = read.delim("DerivedData/CorpusGlobalFeatures.txt")
segments = read.delim("DerivedData/corpus_groove_segments.txt")


# Example 7.1a, The OtherSide transcription (see MuseScore file) ----
# Example 7.1b, The OtherSide, mm. 1:8

PlotVerse("theOtherSide1",m.range = 0:7)
quartz.save(file="Examples/Chapter 7/Example 7.1b.pdf",type="pdf");dev.off()

# Example 7.2a, Kool On transcription (see MuseScore file)----------

# Example 7.1b, Kool On, mm. 1:8

PlotVerse("koolOn2",m.range = 0:7)
quartz.save(file="Examples/Chapter 7/Example 7.2b.pdf",type="pdf");dev.off()

# Example 7.2c, tabulation of accent in the beat and the flow --------------------------------------------------------------------


beatAccents = list(vocals = c(3,5,8,11,15),
                   guitar1 = c(0,9,11,13,0,9,11,13),
                   guitar2 = c(0,2,6,8,2,4,6,8,10,12),
                   bass = c(0,8,10,12,14,0,4,8,10,14),
                   perc = c(0,2,4,8,10,12,0,2,4,8,10,12))
a = table(c(unlist(beatAccents),0:15))-1
x = corpora %>% filter(verse=="koolOn2") %>% 
  slice(1:99) %>%  # through ..."remain nameless"
  filter(accent==1) %>% 
  .[["beatIndex"]] %>% mod(4)
b = table(c(x,seq(0,3.75,.25)))-1
names(b) = 0:15

quartz(width=4.5,height=1.5)
par(mfcol=c(1,2),mar=c(3.5,3.5,1,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")

barplot(a,col = rep(c("black",rep("gray",3)),4),cex.names=.5,ylim=c(0,8),
        xlab = "Metric position",ylab="Count of accents")
barplot(b,col = rep(c("black",rep("gray",3)),4),cex.names=.5,ylim=c(0,8),
        xlab = "Metric position",ylab="Count of accents")

quartz.save(file="Examples/Chapter 7/Example 7.2c.pdf",type="pdf");dev.off()


# Example 7.3b, Now or Never, mm. 1–8 -------------------------------------

PlotVerse("nowOrNever",m.range = 0:7)
quartz.save(file="Examples/Chapter 7/Example 7.3b.pdf",type="pdf");dev.off()


# Example 7.4: Coincident accents between flow and bass drum --------------
y = laply(seq(-1,1,.25),function(i){
  x = corpora %>% filter(verse=="nowOrNever") %>% 
    slice(3:28) %>%  # through ..."remain nameless"
    filter(accent==1) %>% 
    .[["beatIndex"]] %>% add(i) %>% mod(4)
  b = table(c(x,seq(0,3.75,.25)))-1
  names(b) = 0:15
  p = b[(c(0,2,5,8,11)+1)] %>% sum %>% divide_by(sum(b))
  round(p,digits=2)
})
which(y==min(y))
names(y) = -4:4
quartz(width=3,height=1.5)
par(mar=c(3.5,3.5,2,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
barplot(y,ylab="% accents aligned with bass",xlab="Displacement from performed rhythm (positions)")
quartz.save(file="Examples/Chapter 7/Example 7.4.pdf",type="pdf");dev.off()

# Example 7.5b, I Remember, mm. 1–4 -------------------------------------

PlotVerse("iRemember1",m.range = 0:3)
quartz.save(file="Examples/Chapter 7/Example 7.5b.pdf",type="pdf");dev.off()

# Example 7.6b, I Remember, mm. 5-8 -------------------------------------

PlotVerse("iRemember1",m.range = 4:7)
quartz.save(file="Examples/Chapter 7/Example 7.6b.pdf",type="pdf");dev.off()

# Example 7.7b, Lighthouse, mm. 1-8 -------------------------------------

PlotVerse("lighthouse2",m.range = 0:7)
quartz.save(file="Examples/Chapter 7/Example 7.7b.pdf",type="pdf");dev.off()

# Example 7.7c, I Remember, mm. 9-16 -------------------------------------

PlotVerse("lighthouse2",m.range = 8:15)
quartz.save(file="Examples/Chapter 7/Example 7.7c.pdf",type="pdf");dev.off()


# Example 7.8a: How I Got Over groove segmentation ------------------------


PlotGrooveSegmentation("howIGotOver",r.limit = 0,plot.all.costs = F)
quartz.save(file="Examples/Chapter 7/Example 7.8a.pdf",type="pdf");dev.off()



# Example 7.9, False Media, Flow with Brass -------------------------------
PlotVerse("falseMedia",m.range=-1:8,plot.rhymeClasses = F)
accentInitial = data.frame(meas = c(5,8,11,12),position = c(9,8,0,11), times = c(10,2,5,16))
accent.bi = alply(accentInitial,1,function(i){
  ((((i$meas-1)*4) + (i$position/4))) + seq(0,(i$times-1)*.75,length.out = i$times)
}) %>% unlist %>% unname
points(accent.bi%%4,accent.bi %/% 4 -.35,pch=4)
hairpins = data.frame(meas=c(1,2,3,4,5,8)-1,position=c(rep(4,6)))
for(i in 1:length(hairpins[,1])){
  x = hairpins[i,2]/4
  y = hairpins[i,1]-.35
  lines(c(x,x+.75),c(y,y-.05))
  lines(c(x,x+.75),c(y,y+.05))
}
quartz.save(file="Examples/Chapter 7/Example 7.9a.pdf",type="pdf");dev.off()

PlotVerse("falseMedia",m.range=9:16,plot.rhymeClasses = F)
accentInitial = data.frame(meas = c(5,8,11,12),position = c(9,8,0,11), times = c(10,2,5,16))
accent.bi = alply(accentInitial,1,function(i){
  ((((i$meas-1)*4) + (i$position/4))) + seq(0,(i$times-1)*.75,length.out = i$times)
}) %>% unlist %>% unname
points(accent.bi%%4,accent.bi %/% 4 -.35,pch=4)

quartz.save(file="Examples/Chapter 7/Example 7.9b.pdf",type="pdf");dev.off()

# Example 7.10a, Bread and Butter, Groove Segmentation ---------------------

PlotGrooveSegmentation("breadAndButter2",plot.all.costs = F,r.limit = c(0,.125))
quartz.save(file="Examples/Chapter 7/Example 7.10a.pdf",type="pdf");dev.off()


# # Example 7.10b, Bread and Butter, mm. 1–8 -------------------------------------

PlotVerse("breadAndButter2",m.range = 0:7)
quartz.save(file="Examples/Chapter 7/Example 7.10b.pdf",type="pdf");dev.off()


# Example 7.11a, "Long Time" with percussion------------------------------------

load("SourceData/Other/longTime_percussion.rdata")
l = longTime_perc
PlotVerse("longTime3",plot.rhymeClasses = F,m.range=0:7)
points(l$snare%%4,l$snare %/% 4 -.35,pch=4)
points(l$bass%%4,l$bass %/% 4 -.35,pch=0)
points(l$tom%%4,l$tom %/% 4 -.35,pch=1)
points(l$crash%%4,l$crash %/% 4 -.35,pch=8)
quartz.save(file="Examples/Chapter 7/Example 7.11a.pdf",type="pdf");dev.off()


# Example 7.12 Tempo in “You Got Me" --------------------------------------

x = data.frame(index = 1:8, year = c(1999,1999,1999,2002,2005,2008,2010,2015),
           bpm = c(81,75,75,76,78,85,83,88))

quartz(width=3,height=1.5)
par(mar=c(3.5,3.5,1,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
plot(x$year,x$bpm,xlab="Year",ylab="Beats per minute",ylim=c(75,91))
text(x$year,x$bpm,x$index,font=3,pos = 3)
abline(lm(x$bpm~x$year))
lm(x$bpm~x$year) %>% summary
quartz.save(file="Examples/Chapter 7/Example 7.12.pdf",type="pdf");dev.off()


# Example 7.13, YGM, mm. 13-15 -----------------------------------

PlotVerse("youGotMe1", m.range = 12:14)
quartz.save(file="Examples/Chapter 7/Example 7.13a.pdf",type="pdf");dev.off()
PlotVerse("youGotMe12008a",m.range = 12:14)
quartz.save(file="Examples/Chapter 7/Example 7.13b.pdf",type="pdf");dev.off()


# Example 7.14 Year against % 1mod2 accents -------------------------------

x = corpora %>% 
  filter(grepl("youGotMe1",verse)) %>% 
  filter(accent==1) %>% 
  group_by(verse,onBeat = beatIndex%%.5==0) %>% 
  summarize(n= length(word)) %>% 
  acast(.,verse~onBeat) 
x = cbind(x,p = round(x[,1]/rowSums(x),digits=2),
          year =c(1999,1999,1999,2002,2005,2008,2010,2015),
          bpm = c(81,75,75,76,78,85,83,88) )
x = as.matrix(x)
quartz(width=3,height=1.5)
par(mar=c(3.5,3.5,1,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
plot(x[,"bpm"],x[,"p"],xlab="Beats per minute",ylab="% of 1 (mod 2) accents",ylim=c(.25,.38))
text(x[,"bpm"],x[,"p"],1:8,font=3,pos = 3)
abline(lm(x[,"p"]~x[,"bpm"]))
lm(x[,"p"]~x[,"bpm"]) %>% summary
quartz.save(file="Examples/Chapter 7/Example 7.14.pdf",type="pdf");dev.off()


# Example 7.15, Smoothing over mm. 4-5 ------------------------------------

PlotVerse("youGotMe1", m.range = 3:4)
quartz.save(file="Examples/Chapter 7/Example 7.15a.pdf",type="pdf");dev.off()
PlotVerse("youGotMe12008a",m.range = 3:4)
quartz.save(file="Examples/Chapter 7/Example 7.15b.pdf",type="pdf");dev.off()
PlotVerse("youGotMe12010a",m.range = 3:4)
quartz.save(file="Examples/Chapter 7/Example 7.15c.pdf",type="pdf");dev.off()
PlotVerse("youGotMe12015a",m.range = 3:4)
quartz.save(file="Examples/Chapter 7/Example 7.15d.pdf",type="pdf");dev.off()

# Example 7.16, Smoothing over mm. 11-12 ------------------------------------

PlotVerse("youGotMe1", m.range = 10:11)
quartz.save(file="Examples/Chapter 7/Example 7.16a.pdf",type="pdf");dev.off()
PlotVerse("youGotMe12008a",m.range = 10:11)
quartz.save(file="Examples/Chapter 7/Example 7.16b.pdf",type="pdf");dev.off()
PlotVerse("youGotMe12010a",m.range =10:11)
quartz.save(file="Examples/Chapter 7/Example 7.16c.pdf",type="pdf");dev.off()
PlotVerse("youGotMe12015a",m.range = 10:11)
quartz.save(file="Examples/Chapter 7/Example 7.16d.pdf",type="pdf");dev.off()
